require 'spec_helper'

describe Run do
  it 'should have a version number' do
    Run::VERSION.should_not be_nil
  end

  it 'should do something useful' do
    false.should eq(true)
  end
end
