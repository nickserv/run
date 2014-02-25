require 'spec_helper'

describe Run do
  it 'has a version number' do
    expect(Run::VERSION).to_not be_nil
  end

  describe '.start' do
    context 'when a valid filename is given with a known extension' do
      it 'runs the file' do
        expect(Run).to receive('system').with('ruby hello.rb')
        Run.start ['hello.rb']
      end
    end
  end
end
