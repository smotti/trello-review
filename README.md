# Description

A small tool to create a weekly review based on cards from trello boards.

# Dependencies

* A list implementation ([SBCL](http://www.sbcl.org/platform-table.html) recommended)
* [Quicklisp](https://www.quicklisp.org/beta/)
* [Python Markdown2](https://github.com/trentm/python-markdown2)

# Installation

```
cd ~/quicklisp/local-projects/
git clone <url>/trello-review.git
```

Login to trello and authorize the app by navigating to the following url:

```
https://trello.com/1/authorize?callback_method=postMessage&scope=read&expiration=never&name=trello-review&key=2e8178984273f4bf22ac309c3f36f9b6
```

This will provide you with a token that never expires, you may change the
expiration specified in the url. Allowed expiration values can be found
[here](https://developers.trello.com/authorize)

Write the returned token to the configuration file, i.e. config.lisp:

```
(:token "thetokenijustreceived"
  ...)
```

# Usage

```
$ ./create-review -config ./config.lisp -boards "Docking Server" -lists "Backlog,Ongoing,Done"
$ markdown2 review.md > review.html

$ ./create-review -boards "Docking Server,Backend - Box Mgmt Context" -lists "Backlog,Ongoing" -output "/tmp/report.md"
$ markdown2 /tmp/report.md > /tmp/report.html
```

Options:

* boards (required):
  * A comma separated list of boards to include in the review
* config (optional)
  * Path to the configuration file that specifies what token to use, the api
    url, etc.
  * Default value is ./config.lisp
* lists (required):
  * A comma separated list of lists from which to fetch cards to include in the
    review
* output (optional):
  * Path where the review (in markdown format) should be stored
  * Default value is ./review.md
* template (optional):
  * Path to the template to use for the review
  * Default value is ./report.tmpl


# Contribute

## Dependencies

* [cl-launch](https://gitlab.common-lisp.net/xcvb/cl-launch/tree/master)
    * To build the executable via create-script.sh

Note that a cl-launch package exists for arch linux in the user repository.
