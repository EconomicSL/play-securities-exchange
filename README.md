# Play application simulating a securities exchange

[![Build Status](https://travis-ci.org/davidrpugh/play-securities-exchange.svg?branch=master)](https://travis-ci.org/davidrpugh/play-securities-exchange)
[![Coverage Status](https://coveralls.io/repos/davidrpugh/play-securities-exchange/badge.svg?branch=master)](https://coveralls.io/r/davidrpugh/play-securities-exchange?branch=master)

A large-scale agent-based simulation model of a securities exchange built using the [Play framework](https://www.playframework.com/)

## Getting started with the Play framework

While the Play framework requires JDK 6+. I would recommend install JDK 8 direct from [Oracle](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html).
Mac, Linux, and Windows users can [download](https://www.playframework.com/download) Play (and its dependencies).   
Mac users can install the Play framework (and dependencies) via Homebrew. Assuming that you have installed Homebrew, then download and install using the following command:

    $ brew install activator

## Code coverage
The project uses the [sbt-scoverage](https://github.com/scoverage/sbt-scoverage) plugin to generate test coverage statistics. To run the tests with coverage enabled simply run the following command from the project root directory:

    $ sbt clean coverage test

After the tests have finished you can then run

    $ sbt coverageReport

to generate the reports. The generated code coverage reports can be found inside `target/scoverage-report`.

## API documentation

The API documentation for the project is very much a work in progress. To generate the most current version of the documentation simply run the `sbt doc` command from the project root directory:

    $ sbt doc
    
The above command generates the documentation and places it under the `target` directory. The root file for the documentation is located at `target/scala-2.11/api/index.html`. To view the documentation, simply open this file in your favorite browser. 

