# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'run/version'

Gem::Specification.new do |spec|
  spec.name          = "run"
  spec.version       = Run::VERSION
  spec.authors       = ["Nicolas McCurdy"]
  spec.email         = ["thenickperson@gmail.com"]
  spec.summary       = "A simple command line tool for running individual " \
                       "files of source code."
  spec.homepage      = ""
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0")
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler", "~> 1.5"
  spec.add_development_dependency "rake"
end
