# Configure and use puget in Cider.

Configure and use [Puget](https://github.com/greglook/puget)
in [Cider](https://github.com/clojure-emacs/cider)
with [Emacs](https://www.gnu.org/software/emacs/).  Cider provides a Puget
already as an option for pretty printing.  In addition there is already
a [lein plugin](https://github.com/greglook/whidbey) to use Puget in a REPL.
So why is this software necessary?

This package provies provides a Puget (and ANSI coloring) in a Cider REPL.
Specifically:

* Puget binds dynamic variables for configuration, which is good, but this
  doesn't allow easy configuration via Cider.  This package allows a one time
  (i.e. REPL startup) to (re)configure.
* Include Emacs Lisp to easily configure the lisp cider project.


## Obtaining

In your `project.clj` file, add:

[![Clojars Project](https://clojars.org/com.zensols.tools/nrepl-puget/latest-version.svg)](https://clojars.org/com.zensols.tools/nrepl-puget/)


## Configuration

In your `profiles.clj` file (or add like configuraiton to `projects.clj`):

```clojure
{:user ...
 :dev {:dependencies [[com.zensols.tools/nrepl-puget "0.0.1"]]}}
```

Create your Puget configuration anywhere you want, say for example
`~/.init.clj`:
```clojure
(require '[zensols.nrpuget.core])

(->> {:print-color true
      :color-scheme {:keyword [:none]
                     :delimiter [:blue]
                     :number [:magenta]
                     :tag [:black]
                     :symbol [:red]
                     :string [:green]
                     :character [:bold :green]
                     :boolean [:black]
                     :nil [:black]}}
     zensols.nrpuget.core/set-options!)
```

Add the [Emacs extension](src/emacs/nrepl-puget.el) to your `load-lib`, like
for example in `~/.emacs.d`.

Next, in your [`~/.emacs`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html) file
add the following (**note** refer to the configuration file you created
earlier):
```emacs
(require 'nrepl-puget)

(defun my-init-cider-connected-hook ()
  (let ((src (-> "~/.init.clj"
		 expand-file-name)))
    (if (file-exists-p src)
	(cider-load-file src))))

(add-hook 'cider-connected-hook 'my-init-cider-connected-hook)

(setq cider-pprint-fn "zensols.nrpuget.core/pprint"
      cider-repl-use-pretty-printing t)
```

## Usage

After you [configure](#configuration) you'll should see Puget formatting in the
REPL output (i.e. results from evaluation and cider functions like
`cider-pprint-eval-last-sexp`.  However, you can optionally get all of your
output through formatting using (for example) `println` but adding the
following to your Clojure source files:

```clojure
(ns example.my-namespace
  (:refer-clojure :exclude [println])
  (:require [zensols.nrpuget.core :refer (println)]))
  
(println ['asym #{"hash" :set} "string" 123])
```

Which produces
```clojure
=> [asym #{"hash" :set} "string" 123]
```


## Documentation

API [documentation](https://plandes.github.io/clj-nrepl-puget/codox/index.html).


## Building

To build from source, do the folling:

- Install [Leiningen](http://leiningen.org) (this is just a script)
- Install [GNU make](https://www.gnu.org/software/make/)
- Install [Git](https://git-scm.com)
- Download the source: `git clone https://github.com/clj-nrepl-puget && cd clj-nrepl-puget`
- Download the make include files:
```bash
mkdir ../clj-zenbuild && wget -O - https://api.github.com/repos/plandes/clj-zenbuild/tarball | tar zxfv - -C ../clj-zenbuild --strip-components 1
```
- Build the software: `make jar`


## Changelog

An extensive changelog is available [here](CHANGELOG.md).


## License

Copyright © 2017 Paul Landes


### Emacs Code

The [Emacs Code](src/emacs) is distributed under the GNU General Public
License, version 3.


### Everything Else

Apache License version 2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
