#!/usr/bin/env python3
"""
Script for running scalafmt and scalafix to prepare a Scala change for commit.

This script is meant for developers using a metals/bloop based Scala
development setup. This can run scalafmt and scalafix without compiling the
project in sbt, making it faster to use when a cached bloop compile exists. For
detailed usage options and examples, run with --help.

Prerequisites:
bloop, scalafmt and scalafix installed and in PATH. `sbt bloopInstall` run at
least once to generate bloop configuration files (This is normally done
automatically by Metals)
"""

import subprocess
import os
from pathlib import Path
import sys
import json
import re
import argparse
from typing import Iterator, Sequence


def parse_arguments():
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description="Run scalafix and scalafmt in diff mode on files changed against a reference",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s                           # Check files changed against origin/main
  %(prog)s --target HEAD~1           # Check files changed against HEAD~1
  %(prog)s --all                     # Check all files (no diff filtering)
  %(prog)s --check                   # Verify formatting and linting without changes
  %(prog)s --target HEAD~1 --check   # Check files against HEAD~1 without making changes
        """,
    )

    parser.add_argument(
        "--target",
        default="origin/main",
        help="Git reference to diff against (default: origin/main)",
    )

    parser.add_argument(
        "--all",
        action="store_true",
        help="Process all files instead of only changed files",
    )

    parser.add_argument(
        "--check",
        action="store_true",
        help="Check only mode - verify formatting and linting without making changes",
    )

    parser.add_argument(
        "--generate-golden",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="Generate golden test files by running test suites with 'Golden' in their name (default: true)",
    )

    return parser.parse_args()


def run_command(
    command: Sequence[str],
    capture_output: bool = False,
    env: dict[str, str] | None = None,
):
    """Runs a command and handles errors."""
    try:
        return subprocess.run(
            command,
            check=True,
            text=True,
            capture_output=capture_output,
            env=env,
        )
    except FileNotFoundError:
        print(
            f"Error: Command '{command[0]}' not found. Is it in your PATH?",
            file=sys.stderr,
        )
        sys.exit(1)
    except subprocess.CalledProcessError as e:
        if capture_output:
            if e.stdout:
                print(e.stdout, file=sys.stderr)
            if e.stderr:
                print(e.stderr, file=sys.stderr)
        sys.exit(e.returncode)


def parse_version_string(version_str: str) -> tuple[int, ...]:
    return tuple(map(int, version_str.split(".")))


def scalafix_cli_version() -> tuple[int, ...]:
    result = run_command(["scalafix", "--version"], capture_output=True)
    version_str = result.stdout.strip()
    return parse_version_string(version_str)


def scalafix_sbt_version(plugin_sbt_path: Path) -> tuple[int, ...]:
    if not plugin_sbt_path.exists():
        print(f"Error: {plugin_sbt_path} not found", file=sys.stderr)
        sys.exit(1)
    plugin_sbt_content = plugin_sbt_path.read_text(encoding="utf-8")
    re_match = re.search(
        r'"ch\.epfl\.scala" % "sbt-scalafix" % "([^"]+)"',
        plugin_sbt_content,
    )
    if re_match is None:
        print(
            f"Error: Could not find sbt-scalafix version in {plugin_sbt_path}",
            file=sys.stderr,
        )
        sys.exit(1)
    return parse_version_string(re_match.group(1))


def validate_scalafix_version(plugin_sbt_path: Path):
    """Validate that the scalafix CLI version is compatible with the sbt-scalafix plugin version."""
    cli_version = scalafix_cli_version()
    sbt_version = scalafix_sbt_version(plugin_sbt_path)
    if cli_version < sbt_version:
        cli_version_str = ".".join(map(str, cli_version))
        sbt_version_str = ".".join(map(str, sbt_version))
        print(
            f"Error: scalafix CLI version '{cli_version_str}' is older than sbt-scalafix version '{sbt_version_str}'.\n"
            f"This can cause runtime errors due to mismatched scalameta versions.\n"
            f"Please update with: cs install scalafix:{sbt_version_str}",
            file=sys.stderr,
        )
        sys.exit(1)


def get_scala_version_from_dependencies(dependencies_content: str) -> str:
    """Extracts a scala version from the Dependencies.scala file content."""
    match = re.search(r'val scala3Version = "([^"]+)"', dependencies_content)
    if not match:
        print(
            'Error: Could not find "scala3Version" in project/Dependencies.scala',
            file=sys.stderr,
        )
        sys.exit(1)
    return match.group(1)


def get_scalafix_dependency_path(
    build_sbt_content: str, scala_binary_version: str
) -> Path:
    """Constructs the path to the scalafix dependency jar."""
    match = re.search(
        r'scalafixDependencies \+= "([^"]+)" %% "([^"]+)" % "([^"]+)"',
        build_sbt_content,
    )
    if not match:
        print(
            "Error: Could not find scalafixDependencies in build.sbt", file=sys.stderr
        )
        sys.exit(1)

    org, name, version = match.groups()
    org_path = org.replace(".", "/")
    artifact_name = f"{name}_{scala_binary_version}"

    return (
        Path.home()
        / ".cache/coursier/v1/https/repo1.maven.org/maven2"
        / org_path
        / artifact_name
        / version
        / f"{artifact_name}-{version}.jar"
    )


def load_bloop_config(bloop_file_path: Path) -> dict | None:
    """Load and parse a bloop JSON configuration file, returning None on failure."""
    try:
        with open(bloop_file_path, "r", encoding="utf-8") as f:
            return json.load(f)
    except json.JSONDecodeError:
        print(f"Warning: Could not parse {bloop_file_path}, skipping.", file=sys.stderr)
    except FileNotFoundError:
        print(
            f"Warning: Bloop file {bloop_file_path} not found, skipping.",
            file=sys.stderr,
        )
    return None


def parse_single_bloop_file(bloop_file_path: Path) -> (str | None, list[str]):
    """Parse a single bloop configuration file to extract semanticdb directory and classpath."""
    semanticdb_dir = None
    classpath_entries = []

    bloop_config = load_bloop_config(bloop_file_path)
    if bloop_config and "project" in bloop_config:
        project = bloop_config["project"]

        # Extract semanticdb target directory
        if "scala" in project and "options" in project["scala"]:
            options = project["scala"]["options"]
            for i, option in enumerate(options):
                # Handle Scala 3 format: -semanticdb-target <path>
                if option == "-semanticdb-target" and i + 1 < len(options):
                    semanticdb_dir = options[i + 1]
                # Handle Scala 2.13 format: -P:semanticdb:targetroot:<path>
                elif option.startswith("-P:semanticdb:targetroot:"):
                    semanticdb_dir = option.split(":", 3)[
                        3
                    ]  # Extract path after -P:semanticdb:targetroot:

        # Extract dependency classpath (compiled JARs)
        if "classpath" in project:
            classpath_entries = project["classpath"]

    return semanticdb_dir, classpath_entries


def get_scala_files_to_process(args, for_scalafix=False):
    """Get the list of Scala files to process based on the arguments.

    Args:
        args: Parsed command-line arguments
        for_scalafix: If True, applies scalafix-specific exclusions
    """
    if args.all:
        tool_name = "scalafix" if for_scalafix else "scalafmt"
        print(f"Processing all files for {tool_name}...")
        all_files = []
        for scala_file in Path(".").rglob("*.scala"):
            relative_path = str(scala_file.relative_to("."))
            if should_include_file(relative_path, for_scalafix):
                all_files.append(relative_path)
        # For scalafmt, also include .sbt files
        if not for_scalafix:
            for sbt_file in Path(".").rglob("*.sbt"):
                relative_path = str(sbt_file.relative_to("."))
                if should_include_file(relative_path, for_scalafix):
                    all_files.append(relative_path)
        return all_files
    else:
        # Get tracked changed files
        changed_files_result = run_command(
            [
                "git",
                "diff",
                "--relative",
                "--name-only",
                "--diff-filter=ACMR",
                args.target,
            ],
            capture_output=True,
        )

        # Get untracked files
        untracked_files_result = run_command(
            ["git", "ls-files", "--others", "--exclude-standard"], capture_output=True
        )

        # Combine tracked changes and untracked files
        all_candidate_files = []
        if changed_files_result.stdout.strip():
            all_candidate_files.extend(changed_files_result.stdout.strip().split("\n"))
        if untracked_files_result.stdout.strip():
            all_candidate_files.extend(
                untracked_files_result.stdout.strip().split("\n")
            )

        # Filter using appropriate filter function
        return [f for f in all_candidate_files if should_include_file(f, for_scalafix)]


def should_include_file(file_path: str, for_scalafix: bool) -> bool:
    """Check if a file should be included in processing.

    Args:
        file_path: The file path to check
        for_scalafix: If True, applies scalafix-specific exclusions (only processes .scala files)
    """
    if (
        not file_path
        or not (file_path.endswith(".scala") or file_path.endswith(".sbt"))
        or file_path.startswith("target/")
        or file_path.startswith(".metals/")
        or "/src_managed/" in file_path
    ):
        return False

    # For scalafix, only process .scala files and apply additional exclusions
    if for_scalafix:
        if (
            not file_path.endswith(".scala")
            or file_path.startswith("project/")
            or file_path.startswith("scalafix/rules/")
            or file_path.startswith("scalafix/input/")
            or file_path.startswith("scalafix/output/")
        ):
            return False

    return True


def parse_bloop_configurations() -> (list[str], list[str]):
    """Parse bloop configuration files to extract semanticdb directories and dependency classpaths."""
    semantic_db_classpath_parts = set()
    dependency_classpath_parts = set()

    for bloop_file in Path(".bloop").glob("*.json"):
        semanticdb_dir, classpath_entries = parse_single_bloop_file(bloop_file)

        if semanticdb_dir:
            semantic_db_classpath_parts.add(semanticdb_dir)

        dependency_classpath_parts.update(classpath_entries)

    # Filter out non-existing paths
    semantic_db_classpath_parts = {
        path for path in semantic_db_classpath_parts if Path(path).exists()
    }
    return list(semantic_db_classpath_parts), list(dependency_classpath_parts)


def iter_bloop_source_files(
    glob_pattern: str = "*.scala", *, test_projects: bool = False
) -> Iterator[tuple[str, Path]]:
    """Yield (project_name, source_file) pairs from bloop source configurations.

    Args:
        glob_pattern: rglob pattern applied to each source directory.
        test_projects: If True, scan *-test.json configs (stripping the -test suffix
                       from the project name). If False, scan non-test configs.
    """
    bloop_dir = Path(".bloop")
    if not bloop_dir.exists():
        return

    for bloop_file in bloop_dir.glob("*.json"):
        is_test = bloop_file.stem.endswith("-test")
        if is_test != test_projects:
            continue
        config = load_bloop_config(bloop_file)
        if not config:
            continue
        project = config.get("project", {})
        project_name = project.get("name", "")
        if test_projects:
            project_name = project_name.removesuffix("-test")
        for source_dir in project.get("sources", []):
            source_path = Path(source_dir)
            if source_path.is_dir():
                yield from ((project_name, f) for f in source_path.rglob(glob_pattern))


def find_golden_test_projects() -> list[str]:
    """Find bloop project names that contain test files with 'Golden' in their name."""
    projects = set()
    for project_name, scala_file in iter_bloop_source_files(test_projects=True):
        if "Golden" in scala_file.stem:
            projects.add(project_name)
    return sorted(projects)


def run_golden_generation(projects: list[str], generate: bool):
    """Run golden test suites, optionally with TYDA_GOLDEN_GENERATE_FILES=1."""
    env = os.environ.copy()
    if generate:
        env["TYDA_GOLDEN_GENERATE_FILES"] = "1"

    run_command(
        ["bloop", "test"] + projects + ["--only", "*Golden*"],
        env=env,
        capture_output=True,
    )


def schema_generator_fqcn(scala_file: Path) -> str | None:
    """Derive the FQCN of a SchemaGenerator from its filename and package declaration."""
    for line in scala_file.read_text(encoding="utf-8").splitlines():
        if (stripped := line.strip()).startswith("package "):
            return f"{stripped[len('package '):].strip()}.{scala_file.stem}"
    return None


def find_bloop_projects_for_files(files: list[str]) -> list[str]:
    """Find bloop project names whose source trees contain any of the given files."""
    resolved_files = {Path(f).resolve() for f in files}
    projects = set()

    for bloop_file in Path(".bloop").glob("*.json"):
        config = load_bloop_config(bloop_file)
        if not config:
            continue
        project = config.get("project", {})
        project_name = project.get("name", "")
        for source_dir in project.get("sources", []):
            source_path = Path(source_dir).resolve()
            if not source_path.is_dir():
                continue
            for resolved_file in resolved_files:
                try:
                    resolved_file.relative_to(source_path)
                    projects.add(project_name)
                    break
                except ValueError:
                    continue

    return sorted(projects)


def main():
    """Main function."""
    args = parse_arguments()

    # Find git repository root and change to it
    git_root_result = run_command(
        ["git", "rev-parse", "--show-toplevel"], capture_output=True
    )
    git_root = Path(git_root_result.stdout.strip())

    os.chdir(git_root)

    dependencies_path = Path("project/Dependencies.scala")
    if not dependencies_path.exists():
        print("Error: project/Dependencies.scala not found", file=sys.stderr)
        sys.exit(1)
    dependencies_content = dependencies_path.read_text(encoding="utf-8")

    scala3_version = get_scala_version_from_dependencies(dependencies_content)
    scala_binary_version = "2.13"

    build_sbt_path = Path("build.sbt")
    if not build_sbt_path.exists():
        print("Error: build.sbt not found", file=sys.stderr)
        sys.exit(1)
    build_sbt_content = build_sbt_path.read_text(encoding="utf-8")

    scalafix_rules_classes_dir = (
        ".bloop/scalafixRules/bloop-bsp-clients-classes/classes-bloop-cli"
    )
    external_scalafix_rules = get_scalafix_dependency_path(
        build_sbt_content, scala_binary_version
    )

    if not external_scalafix_rules.exists():
        print(
            f"Error: Scalafix rules jar not found at {external_scalafix_rules}",
            file=sys.stderr,
        )
        print(
            "You might need to run `sbt` once to download dependencies.",
            file=sys.stderr,
        )
        sys.exit(1)

    # Get scalafix rules dependencies from bloop configuration
    scalafix_bloop_file = Path(".bloop/scalafixRules.json")
    scalafix_rules_deps = []
    if scalafix_bloop_file.exists():
        _, scalafix_classpath_entries = parse_single_bloop_file(scalafix_bloop_file)
        scalafix_rules_deps = scalafix_classpath_entries

    # Build complete scalafix rules classpath including dependencies
    scalafix_rules_classpath_parts = [
        scalafix_rules_classes_dir,
        str(external_scalafix_rules),
    ] + scalafix_rules_deps
    scalafix_rules_classpath = ":".join(scalafix_rules_classpath_parts)

    # Get files for scalafix (with scalafix-specific exclusions)
    scalafix_files = get_scala_files_to_process(args, for_scalafix=True)

    # Get files for scalafmt (with only common exclusions)
    scalafmt_files = get_scala_files_to_process(args, for_scalafix=False)

    # Run scalafix first (if there are files to process)
    if scalafix_files:
        if args.all:
            print("Running bloop to compile project...")
            run_command(["bloop", "compile", "root", "root-test"])
        else:
            projects_to_compile = find_bloop_projects_for_files(scalafix_files)
            if not projects_to_compile:
                print(
                    "Error: Could not determine bloop projects for changed files. Try running `sbt bloopInstall`.",
                    file=sys.stderr,
                )
                sys.exit(1)
            if "scalafixRules" not in projects_to_compile:
                projects_to_compile.append("scalafixRules")
            print(f"Running bloop compile on: {', '.join(projects_to_compile)}...")
            run_command(["bloop", "compile"] + projects_to_compile)

        validate_scalafix_version(Path("project/plugin.sbt"))

        semantic_db_classpath_parts, dependency_classpath_parts = (
            parse_bloop_configurations()
        )

        if not semantic_db_classpath_parts:
            print(
                "Error: Could not find any existing semanticdb directories in .bloop/*.json files.",
                file=sys.stderr,
            )
            sys.exit(1)

        # Combine semanticdb directories and dependency JARs for full classpath
        full_classpath = semantic_db_classpath_parts + dependency_classpath_parts
        full_classpath_str = ":".join(full_classpath)

        scalafix_command = [
            "scalafix",
            "--config",
            ".scalafix.conf",
            "--tool-classpath",
            scalafix_rules_classpath,
            "--classpath",
            full_classpath_str,
            "--scala-version",
            scala3_version,
            "--scalac-options",
            "-Wunused:all",
            "--sourceroot",
            ".",
        ]

        if args.check:
            scalafix_command.append("--check")

        scalafix_command.extend(scalafix_files)

        if args.all:
            print(f"Running scalafix on {len(scalafix_files)} file(s)...")
        else:
            print(f"Running scalafix on {len(scalafix_files)} changed file(s)...")
        run_command(scalafix_command)
    else:
        if args.all:
            print("No Scala files found for scalafix")
        else:
            print(f"No Scala files changed against {args.target} for scalafix")

    # Run scalafmt after scalafix
    if scalafmt_files:
        scalafmt_command = ["scalafmt"]
        if args.check:
            scalafmt_command.append("--test")

        if args.all:
            print(f"Running scalafmt on {len(scalafmt_files)} file(s)...")
        else:
            print(f"Running scalafmt on {len(scalafmt_files)} changed file(s)...")

        run_command(scalafmt_command + scalafmt_files)
    else:
        if args.all:
            print("No files found for scalafmt")
        else:
            print(f"No files changed against {args.target} for scalafmt")

    if args.generate_golden:
        golden_projects = find_golden_test_projects()
        if golden_projects:
            print(f"Running golden test generation for: {', '.join(golden_projects)}")
            run_golden_generation(golden_projects, generate=not args.check)
        else:
            print("No projects with golden test suites found.")

    if args.check:
        print(
            "Code linting and formatting check complete! (check mode - no changes made)"
        )
    else:
        print("Code linting and formatting complete!")


if __name__ == "__main__":
    main()
