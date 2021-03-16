module.exports = function(config) {
  var junitOutputDir = process.env.CIRCLE_TEST_REPORTS || "target/junit";
  config.set({
    browsers: ["ChromeHeadless", "FirefoxHeadless"],
    // The directory where the output file lives
    basePath: "target",
    // The file itself
    files: ["ci.js"],
    frameworks: ["cljs-test"],
    reporters: ["spec"],
    specReporter: {
      maxLogLines: 5, // limit number of lines logged per test
      suppressErrorSummary: true, // do not print error summary
      suppressFailed: false, // do not print information about failed tests
      suppressPassed: false, // do not print information about passed tests
      suppressSkipped: true, // do not print information about skipped tests
      showSpecTiming: false // print the time elapsed for each spec
    },
    plugins: [
      "karma-cljs-test",
      "karma-spec-reporter",
      "karma-chrome-launcher",
      "karma-firefox-launcher",
      "karma-junit-reporter"
    ],
    colors: true,
    logLevel: config.LOG_INFO,
    client: {
      args: ["shadow.test.karma.init"],
      singleRun: true
    },
    // the default configuration
    junitReporter: {
      outputDir: junitOutputDir + "/karma", // results will be saved as outputDir/browserName.xml
      outputFile: undefined, // if included, results will be saved as outputDir/browserName/outputFile
      suite: "" // suite will become the package name attribute in xml testsuite element
    }
  });
};
