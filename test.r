require('hash')

config.description <- hash(
                           list(
                                "wd"=c(key="--wd=", description="Working dir"),
                                "outDir"=c(key="--out-dir=", description="Output directory"),
                                "target"=c(key="--target=", description="Output target")))



c( "/Library/Frameworks/R.framework/Resources/bin/exec/R", "--slave", "--no-restore", "--file=test.r", "--args", "--out=pdf_document", "--wd=.")
#

