require 'spec_helper'

describe Run do
  it 'has a version number' do
    expect(Run::VERSION).to_not be_nil
  end

  describe '.start' do
    context 'when a filename is given with a known extension' do
      it 'runs the file' do
        expect(Run).to receive('system').with('ruby hello.rb')
        Run.start ['hello.rb']
      end
    end

    context 'when a filename is given without a known extension' do
      it { expect { Run.start ['hello.unknown'] }.to raise_error RuntimeError }
    end

    context 'when a filename is given without any extension' do
      it { expect { Run.start ['hello'] }.to raise_error RuntimeError }
    end

    context 'when no filename is given' do
      it { expect { Run.start [] }.to raise_error RuntimeError }
    end
  end
end
