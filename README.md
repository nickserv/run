# Run
[![Build Status](https://travis-ci.org/nicolasmccurdy/run.png)](https://travis-ci.org/nicolasmccurdy/run)

A simple command line tool for running individual files of source code.

Run is kind of like [open](https://developer.apple.com/library/mac/documentation/Darwin/Reference/ManPages/man1/open.1.html) for Mac or [xdg-open](https://wiki.archlinux.org/index.php/xdg-open) for Linux, except it tries to always run source code files instead of just opening them in text editors. Run can also automatically compile source code for languages that need to be compiled.

## Installation
Run is in development and will be available on [rubygems.org](https://rubygems.org/) when it is stable, though you can still try it out now.
```bash
git clone https://github.com/nicolasmccurdy/run
cd run
bundle
rake install
```

## Usage
`run <filename>`

### Notes:
1. For now, the file must have an extension.
2. Shebang lines are currently ignored.
3. The main implementation of the file's programming language must be installed for it to run.
4. You can see the list of supported programming languages in [commands.yml](https://github.com/nicolasmccurdy/run/blob/master/lib/commands.yml).

## Contributing

1. Fork it ( http://github.com/<my-github-username>/run/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
