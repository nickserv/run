require "run/version"
require 'yaml'

module Run
  COMMANDS = YAML.load_file('lib/commands.yml')

  def self.start(args)
    path = args[0]
    extension = File.extname path

    if extension != ''
      extension.sub!('.', '')
      system COMMANDS[extension].gsub('%', path)
    end
  end
end
