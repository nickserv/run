import Test.Hspec

main :: IO ()
main = hspec $
  describe "Run" $
    describe ".commandForFile" $
      context "when a filename is given with a known extension" $
        it "should be a valid command" $
          Run.commandForFile "hello.rb" `shouldBe` Just "ruby hello.rb"

      context "when a filename is given without a known extension" $
        Run.commandForFile "hello.unknown" `shouldBe` Nothing

      context "when a filename is given without any extension" $
        Run.commandForFile "hello" `shouldBe` Nothing

    describe ".start" $
      context "when a filename is given with a known extension" $
        it "runs the file" $
          --expect(Run).to receive('system').with('ruby hello.rb')
          Run.start ["hello.rb"]

      context "when a filename is given without a known extension" $
        Run.start ["hello.unknown"] `shouldThrow` anyException

      context "when a filename is given without any extension" $
        Run.start ["hello"] `shouldThrow` anyException

      context "when no filename is given" $
        Run.start [] `shouldThrow` anyException
