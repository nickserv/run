require 'run/version'
require 'json'

module Run
  COMMANDS = JSON.parse IO.read 'lib/commands.json'

  def self.command_for_file(path)
    extension = File.extname(path).sub('.', '')

    COMMANDS[extension].sub('%', path) if COMMANDS.key?(extension)
  end

  def self.start(args)
    fail 'No files given.' if args == []
    command = command_for_file args[0]

    if command.nil?
      fail 'Run could not determine how to run this file because it does ' \
           'not have a known extension.'
    else
      system command
    end
  end
end
