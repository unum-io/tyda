#!/usr/bin/env python3

import argparse
import os
import re
import subprocess
import sys
from pathlib import Path


TAG_RE = re.compile(r"^v(?P<major>\d+)\.(?P<minor>\d+)\.\d+$")
TL_BASE_VERSION_RE = re.compile(
    r'^(ThisBuild / tlBaseVersion := ")[^"]*(")',
    re.MULTILINE,
)


def run(
    *args: str,
    check: bool = True,
    capture: bool = False,
) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(
        args,
        text=True,
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.STDOUT if capture else None,
    )

    if check and result.returncode != 0:
        output = result.stdout or ""
        raise RuntimeError(
            f"Command failed with exit code {result.returncode}: {' '.join(args)}\n{output}"
        )

    return result


def parse_release_tag(value: str) -> tuple[int, int]:
    match = TAG_RE.fullmatch(value)

    if not match:
        print("--------------------------------")
        print(f"Skipping invalid tag: {value or '<empty>'}")
        print("--------------------------------")
        sys.exit(1)

    return int(match["major"]), int(match["minor"])


def next_base_version(release_tag: str) -> str:
    major, minor = parse_release_tag(release_tag)
    return f"{major}.{minor + 1}"


def update_tl_base_version(path: Path, version: str):
    content = path.read_text()

    if not TL_BASE_VERSION_RE.search(content):
        raise RuntimeError(f"Could not find tlBaseVersion setting in {path}")

    updated = TL_BASE_VERSION_RE.sub(
        rf"\g<1>{version}\2",
        content,
    )

    if updated == content:
        print("--------------------------------")
        print(f"No changes. tlBaseVersion is already {version}.")
        print("--------------------------------")
        sys.exit(0)

    path.write_text(updated)


def upstream_repo_for(repo: str) -> str:
    return run(
        "gh",
        "repo",
        "view",
        repo,
        "--json",
        "nameWithOwner,parent",
        "--jq",
        r'if .parent then "\(.parent.owner.login)/\(.parent.name)" else .nameWithOwner end',
        capture=True,
    ).stdout.strip()


def origin_repo() -> str:
    return run(
        "gh",
        "repo",
        "view",
        run("git", "remote", "get-url", "origin", capture=True).stdout.strip(),
        "--json",
        "nameWithOwner",
        "--jq",
        ".nameWithOwner",
        capture=True,
    ).stdout.strip()


def create_or_report_pr(branch: str, version: str, release_tag: str) -> None:
    or_repo = origin_repo()
    or_owner = or_repo.split("/", 1)[0]
    us_repo = upstream_repo_for(or_repo)

    body = (
        f"Derived from release tag {release_tag}. "
        f"Patch ignored. Next base version: {version}."
    )

    create = run(
        "gh",
        "pr",
        "create",
        "--repo",
        us_repo,
        "--title",
        f"Update tlBaseVersion to {version}",
        "--body",
        body,
        "--base",
        "main",
        "--head",
        f"{or_owner}:{branch}",
        check=False,
        capture=True,
    )

    if create.returncode == 0:
        print("--------------------------------")
        print("PR Created")
        print(create.stdout, end="")
        print("--------------------------------")
        return

    existing = run(
        "gh",
        "search",
        "prs",
        f"head:{branch}",
        "--repo",
        us_repo,
        "--json",
        "url",
        "--jq",
        '.[].url // ""',
        capture=True,
    ).stdout.strip()

    if existing:
        print("--------------------------------")
        print(f"PR already exists:\n{existing}")
        print("--------------------------------")
        return

    print(create.stdout, file=sys.stderr, end="")
    sys.exit(create.returncode)


def remote_branch_oid(branch: str) -> str | None:
    result = run(
        "git",
        "ls-remote",
        "--heads",
        "origin",
        branch,
        check=False,
        capture=True,
    )

    if result.returncode != 0:
        raise RuntimeError(result.stdout or "git ls-remote failed")

    line = result.stdout.strip()
    return line.split()[0] if line else None


def push_branch(branch: str) -> None:
    oid = remote_branch_oid(branch)

    if oid is None:
        # Remote branch does not exist; create it normally.
        run("git", "push", "origin", f"HEAD:refs/heads/{branch}")
    else:
        # Remote branch exists; only overwrite if it is still at the OID we saw.
        run(
            "git",
            "push",
            f"--force-with-lease=refs/heads/{branch}:{oid}",
            "origin",
            f"HEAD:refs/heads/{branch}",
        )


def get_remote_name(repo: str):
    output = run(
        "git",
        "remote",
        "-v",
        check=True,
        capture=True,
    ).stdout

    for line in output.splitlines():
        parts = line.split()

        if len(parts) < 3:
            continue

        name, url, operation = parts[:3]

        if operation != "(fetch)":
            continue

        if url.endswith(repo):
            return name


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "release_tag",
        nargs="?",
        default=os.environ.get("GITHUB_RELEASE_TAG", ""),
        help="Release tag to base tlBaseVersion on ( vX.Y.Z -> tlBaseVersion := X.{Y+1} )",
    )
    parser.add_argument(
        "--switch-back",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="Switch back to the previous Git checkout target before exiting.",
    )
    args = parser.parse_args()
    switched_branch = False

    # Find git repository root and change to it
    git_root_result = run("git", "rev-parse", "--show-toplevel", capture=True)
    git_root = Path(git_root_result.stdout.strip())
    os.chdir(git_root)

    version = next_base_version(args.release_tag)
    branch = f"bump-base-version-{version}"

    print(f"Release tag: {args.release_tag}")
    print(f"Next tlBaseVersion: {version}")
    print(f"Branch: {branch}")

    try:
        remote_name = get_remote_name("unum-io/tyda.git") or "origin"
        run("git", "fetch", remote_name, "main")
        run("git", "switch", "-C", branch, f"{remote_name}/main")
        switched_branch = True

        update_tl_base_version(Path("build.sbt"), version)

        run("git", "add", "build.sbt")
        run("git", "commit", "-m", f"Update tlBaseVersion to {version}")
        push_branch(branch)

        create_or_report_pr(branch, version, args.release_tag)

        return 0

    finally:
        if switched_branch and args.switch_back:
            run("git", "checkout", "-", check=False)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except RuntimeError as exc:
        print(exc, file=sys.stderr)
        sys.exit(1)
