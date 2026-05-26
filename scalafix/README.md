# Scalafix
This project defines custom semantic refactoring and linting rules to be used in all our scala projects.

## Apply a rule
To apply all rules to the codebase, run
```
sbt scalafix 
```

To apply specific rules to the codebase, run 
```
sbt scalafix MyRule1 MyRule2
```

To apply a specific rule to a specific project, run
```
sbt someProject/scalafix MyRule
```



## Run test suite
To test our existing rules, run
```
sbt scalafixTests/test
```


## Create new rule
To create a new rule:

- Create a new file with the rule implementation: `rules/src/main/scala/com/choreograph/scalafix/MyRule.scala`
- Append the following line to `rules/src/main/resources/META-INF/services/scalafix.v1.Rule`:
    ```
    com.choreograph.scalafix.MyRule
    ```
- Test the rule:
    - Create an input file that tests the behaviour of your new rule: `input/main/scala/com/choreograph/scalafix/MyRule.scala`. The first line of the input file should be a comment referencing your new rule:
        ```
        /* rule = MyRule */
        ```
    - Add the corresponding expected output after applying the rule to `output/main/scala/com/choreograph/scalafix/MyRule.scala`
    - Test that it works, by running  
        ```
        sbt scalafixTests/test
        ```
