#build, test and copy executable from dist folder to sandbox
#!/bin/bash
cd ..
cabal configure
cabal build
cabal test
cp dist/build/markdown_monparsing/markdown_monparsing sandbox/mdConvert
cp dist/test/markdown-monparsing-0.1.0.0-Commom-Parsers-Test.log sandbox/test.log
cd sandbox 
