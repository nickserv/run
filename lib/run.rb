require "run/version"
require 'yaml'

module Run
  COMMANDS = YAML.load_file('lib/commands.yml')

  def self.start(args)
    fail RuntimeError, 'No files given.' if args == []
    path = args[0]
    extension = File.extname path

    if extension == ''
      fail RuntimeError, 'Run could not determine how to run this file ' \
                         'because it has no extension.'
    else
      extension.sub!('.', '')
      fail RuntimeError, 'Run could not determine how to run this file ' \
                         'because it has an unknown extension.' \
                         unless COMMANDS.key? extension
      system COMMANDS[extension].gsub('%', path)
    end
  end
end
