require 'run/version'
require 'yaml'

module Run
  COMMANDS = YAML.load_file('lib/commands.yml')

  def self.start(args)
    fail 'No files given.' if args == []
    path = args[0]
    extension = File.extname path

    if extension == ''
      fail 'Run could not determine how to run this file because it has no ' \
           'extension.'
    else
      extension.sub!('.', '')
      fail 'Run could not determine how to run this file because it has an ' \
           'unknown extension.' unless COMMANDS.key? extension
      system COMMANDS[extension].gsub('%', path)
    end
  end
end
