# Scala-Plotalyzer

This tool evaluates the data collected by the scala-appanalyzer and outputs summary jsons containing the interesting data points for final processing such as plotting or writing.

### Installation

```
$> sbt stage
...
```

Now you need to adapt the `db.json` which contains the credentials for the database and is the first parameter for execution.


#### Using Scala-Plotalyzer

The first parameter is the path to the db conf. The second the id of the experiment ot be analyzed. The third parameter is the output path for the json file to be created (file path required!).
Afterwards you choose an action to execute. Each action is documented within the app and provides information via the `-h` flag.

E.g., to analyze the consent dialogs collected run:

```
$> ./scala-plotalyzer db.conf <ID> ./consent-test.json consentDialog analyze
...
```