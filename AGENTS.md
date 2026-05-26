# AGENTS.md

## Building and testing

Project names are camelCased, derived from the directory name (e.g. `tyda-spark` → `tydaSpark`). When in doubt check `build.sbt` for the exact project name.

Run a full project test suite:
```
bloop test <project>
```

Filter by test class and/or test name:
```
bloop test <project> --only '*ClassName*' -- -z 'test name substring'
```

After changing the build or adding dependencies, regenerate the bloop files:
```
sbt bloopInstall
```

## Formatting and linting

Before committing, run scalafmt and scalafix on changed files:
```
./bloop_prepare.py
```

This script runs scalafix then scalafmt on files changed relative to `origin/main`, then regenerates golden files automatically.

To run on all Scala files instead of only changed ones:
```
./bloop_prepare.py --all
```

## Code style

- Never add comments that merely restate the code. Rarely use comments to divide sections of code — prefer extracting methods and classes instead.
- Opaque types are opaque for a reason. Do not depend on internal details outside the defining module. Using `asInstanceOf` to construct or deconstruct opaque types is forbidden.
- Use functional patterns when possible.
- Only use `asInstanceOf` as a last resort. Any such usage must be accompanied by a `// TYPE SAFETY:` comment explaining why it is safe.
- Use `scala.util.Using` over `try/finally` for resource management.
