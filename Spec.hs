import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Run" $ do
    describe ".command_for_file" $ do
      context "when a filename is given with a known extension" $ do
        it "should be a valid command" $ do
          Run.commandForFile "hello.rb" `shouldBe` Just "ruby hello.rb"

      context "when a filename is given without a known extension" $ do
        Run.command_for_file "hello.unknown" `shouldBe` Nothing

      context "when a filename is given without any extension" $ do
        Run.command_for_file "hello" `shouldBe` Nothing

    describe ".start" $ do
      context "when a filename is given with a known extension" $ do
        it "runs the file" $ do
          --expect(Run).to receive('system').with('ruby hello.rb')
          Run.start ["hello.rb"]

      context "when a filename is given without a known extension" $ do
        Run.start ["hello.unknown"] `shouldThrow` anyException

      context "when a filename is given without any extension" $ do
        Run.start ["hello"] `shouldThrow` anyException

      context "when no filename is given" $ do
        Run.start [] `shouldThrow` anyException
