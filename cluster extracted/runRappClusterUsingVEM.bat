set folder= %~dp0


set _relativeAdd=%folder%\bin\x64


CD %_relativeAdd%
Rscript %folder%\RScriptCluster\tmLDAwithUserInput_VEM.R


pause




