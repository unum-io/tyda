#!/usr/bin/env python3
import sys
import os
import zipfile
import re
import argparse
import subprocess


def build_coursier_artifact_map(spark_version, scala_version):
    """
    Builds an in-memory dictionary mapping artifactId -> groupId
    by checking what Coursier resolved for the standard Spark modules.
    This acts as a lookup table for jars that have no internal metadata.
    """
    print(
        "Building Coursier artifact-to-groupId dictionary for fallback lookups...",
        file=sys.stderr,
    )
    spark_modules = [
        "core", "sql", "hive", "mllib", "streaming",
        "graphx", "repl", "kubernetes", "yarn"
    ]
    modules = [
        f"org.apache.spark:spark-{m}_{scala_version}:{spark_version}"
        for m in spark_modules
    ]

    try:
        result = subprocess.run(
            ["cs", "resolve"] + modules, capture_output=True, text=True, check=True
        )

        mapping = {}
        for line in result.stdout.strip().split("\n"):
            parts = line.split(":")
            if len(parts) >= 3:
                group, artifact = parts[0], parts[1]
                mapping[artifact] = group

        return mapping
    except FileNotFoundError:
        print(
            "Error: 'cs' not found. Make sure Coursier is installed.",
            file=sys.stderr,
        )
        sys.exit(1)
    except subprocess.CalledProcessError as e:
        print(
            f"Error: cs resolve returned {e.returncode}.",
            file=sys.stderr,
        )
        if e.stderr:
            print(e.stderr.strip(), file=sys.stderr)
        sys.exit(1)


def resolve_jar_coords(jar_path, filename):
    """Extracts Maven coordinates from pom.properties, falling back to filename heuristics."""
    try:
        with zipfile.ZipFile(jar_path, "r") as jar:
            pom_props = [
                entry
                for entry in jar.namelist()
                if entry.startswith("META-INF/maven/") and entry.endswith("/pom.properties")
            ]
            if pom_props:
                content = jar.read(pom_props[0]).decode("utf-8")
                props = {}
                for line in content.split("\n"):
                    if "=" in line:
                        key, val = line.strip().split("=", 1)
                        props[key] = val
                group, artifact, version = props.get("groupId"), props.get("artifactId"), props.get("version")
                if group and artifact and version:
                    return group, artifact, version
    except Exception as e:
        print(f"Error reading JAR {jar_path}: {e}", file=sys.stderr)
        sys.exit(1)

    name = filename[:-4]
    match = re.match(r"^(.+?)-(\d+(?:\.\d+)*(?:\.[a-zA-Z0-9_-]+)*.*)$", name)
    if match:
        return None, match.group(1), match.group(2)
    return None, None, None


def process_jars_directory(jars_dir, spark_version, scala_version):
    """Scans a directory of JARs and generates the SBT Seq."""
    if not os.path.isdir(jars_dir):
        print(f"Error: {jars_dir} is not a valid directory.", file=sys.stderr)
        sys.exit(1)

    coursier_map = build_coursier_artifact_map(spark_version, scala_version)
    print(f"Scanning JARs in {jars_dir}...", file=sys.stderr)

    overrides = set()
    unresolved = []

    for filename in sorted(os.listdir(jars_dir)):
        if not filename.endswith(".jar"):
            continue

        jar_path = os.path.join(jars_dir, filename)

        if filename.startswith(("scala-library-", "scala-compiler-", "scala-reflect-")):
            continue

        group, artifact, version = resolve_jar_coords(jar_path, filename)

        if not group and artifact:
            group = coursier_map.get(artifact)

        if group == "org.apache.spark":
            continue

        if group and artifact and version:
            overrides.add(f'  "{group}" % "{artifact}" % "{version}"')
        else:
            unresolved.append((filename, group, artifact, version))

    if unresolved:
        print(
            "\nError: Could not completely resolve Maven coordinates for the following JARs:",
            file=sys.stderr,
        )
        for u_filename, u_group, u_artifact, u_version in unresolved:
            print(
                f"  - {u_filename} (Extracted: Group={u_group}, Artifact={u_artifact}, Version={u_version})",
                file=sys.stderr,
            )
        sys.exit(1)

    return overrides


def main():
    parser = argparse.ArgumentParser(
        description="Generate SBT dependency overrides directly from a Spark distribution directory."
    )
    parser.add_argument(
        "spark_path",
        type=str,
        help="Path to the Spark distribution directory (e.g. /opt/spark) OR the specific /jars directory.",
    )
    parser.add_argument(
        "spark_version",
        type=str,
        help="The Spark version being parsed (e.g. 3.5.5) required for fallback lookup",
    )
    parser.add_argument(
        "scala_version",
        type=str,
        help="The Scala binary version (e.g. 2.13) required for fallback lookup",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=str,
        default="project/SparkDependencies.scala",
        help="Optional output file to write the SBT Seq to. Defaults to project/SparkDependencies.scala",
    )

    args = parser.parse_args()

    jars_dir = args.spark_path
    if os.path.isdir(os.path.join(args.spark_path, "jars")):
        jars_dir = os.path.join(args.spark_path, "jars")

    overrides = process_jars_directory(jars_dir, args.spark_version, args.scala_version)

    sorted_overrides = sorted(overrides)

    script_name = os.path.basename(__file__)
    spark_folder_name = os.path.basename(os.path.normpath(args.spark_path))
    output_lines = [
        "// AUTO-GENERATED — DO NOT EDIT DIRECTLY",
        f"// Generated by: {script_name} <{spark_folder_name}> {args.spark_version} {args.scala_version}",
        "// Re-run the script to regenerate after upgrading Spark.",
        "",
        "import sbt.*",
        "",
        "// scala-steward:off",
        "object SparkDependencies {",
        "  lazy val dependencies = Seq(",
    ]

    for line in sorted_overrides:
        output_lines.append(f"  {line},")

    output_lines.extend(["  )", "}"])

    final_output = "\n".join(output_lines) + "\n"

    if args.output and args.output != "-":
        try:
            os.makedirs(os.path.dirname(os.path.abspath(args.output)), exist_ok=True)
            with open(args.output, "w") as f:
                f.write(final_output)
            print(
                f"Successfully wrote dependency configuration to {args.output}",
                file=sys.stderr,
            )
        except IOError as e:
            print(f"Failed to write to file {args.output}: {e}", file=sys.stderr)
            sys.exit(1)
    else:
        print(final_output)


if __name__ == "__main__":
    main()
