# Play application simulating a securities exchange

[![Build Status](https://travis-ci.org/davidrpugh/play-securities-exchange.svg?branch=master)](https://travis-ci.org/davidrpugh/play-securities-exchange)
[![Coverage Status](https://coveralls.io/repos/davidrpugh/play-securities-exchange/badge.svg?branch=master)](https://coveralls.io/r/davidrpugh/play-securities-exchange?branch=master)

A large-scale agent-based simulation model of a securities exchange.

## Code coverage
The project uses the [sbt-scoverage](https://github.com/scoverage/sbt-scoverage) plugin to generate test coverage statistics. To run the tests with coverage enabled simply run the `sbt clean coverage test` command from the project root directory:

    $ sbt clean coverage test

After the tests have finished you can then run

    $ sbt coverageReport

to generate the reports. The generated code coverage reports can be found inside `target/scoverage-report`.

## API documentation

The API documentation for the project is very much a work in progress. To generate the most current version of the documentation simply run the `sbt doc` command from the project root directory:

    $ sbt doc
    
The above command generates the documentation and places it under the `target` directory. The root file for the documentation is located at `target/scala-2.11/api/index.html`. To view the documentation, simply open this file in your favorite browser. 

