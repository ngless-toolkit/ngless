{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Tests.Select
    ( tgroup_Select
    ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB

import Interpretation.Select (fixCigar)
import Data.Sam
import Tests.Utils
import Utils.Here


tgroup_Select = $(testGroupGenerator)


samLineFlat = [here|IRIS:7:3:1046:1723#0	4	*	0	0	40M	*	0	0	AAAAAAAAAAAAAAAAAAAATTTAAA	aaaaaaaaaaaaaaaaaa`abbba`^	AS:i:0	XS:i:0	NM:i:1|]
samLine = SamLine
            { samQName = "IRIS:7:3:1046:1723#0"
            , samFlag = 4
            , samRName = "*"
            , samPos = 0
            , samMapq = 0
            , samCigar = "40M"
            , samRNext = "*"
            , samPNext = 0
            , samTLen = 0
            , samSeq = "AAAAAAAAAAAAAAAAAAAATTTAAA"
            , samQual = "aaaaaaaaaaaaaaaaaa`abbba`^"
            , samExtra = "AS:i:0\tXS:i:0\tNM:i:1"
            }
simple = [here|
simulated:1:1:38:663#0	0	Ref1	1018	3	69M16S	=	1018	0	TTCGAGAAGATGGGTATCGTGGGAAATAACGGAACGGGGAAGTCTACCTTCATCAAGATGCTGCTGGGCTTGGTGAAACCCGACA	IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII	NM:i:5	MD:Z:17T5T14A2A2G24	AS:i:44	XS:i:40|]

complex = [here|
SRR070372.3	16	V	7198336	21	26M3D9M3D6M6D8M2D21M	*	0	0	CCCTTATGCAGGTCTTAACACAATTCTTGTATGTTCCATCGTTCTCCAGAATGAATATCAATGATACCAA	014<<BBBBDDFFFDDDDFHHFFD?@??DBBBB5555::?=BBBBDDF@BBFHHHHHHHFFFFFD@@@@@	NM:i:14	MD:Z:26^TTT9^TTC6^TTTTTT8^AA21	AS:i:3	XS:i:0|]

refinsert = [here|
SRR6028238.2619770	417	X	1319005	0	5M1I40M54H	=	2019245	700291	TTTTCCGCTGAATATGCCCAAAGTGCAACAACGACGACCGCCGCCA	@DDDEDDDDBDDDEEECDDDDDCAACCCCBBDDDBBBBBB<@BBDB	NM:i:1	MD:Z:45	MC:Z:51M50H	AS:i:40|]

case_read_one_Sam_Line = readSamLine samLineFlat @?= Right samLine
case_encode = (BL.toStrict . BB.toLazyByteString . encodeSamLine $ samLine) @?= samLineFlat

case_isAligned_raw = isAligned (fromRight . readSamLine $ complex) @? "Should be aligned"
case_match_identity_soft = fromRight (matchIdentity True samLine) == 0.975 @? "Soft clipped read (low identity)"

case_matchSize1 = fromRight (matchSize True =<< readSamLine complex) @?= (26+  9+  6+  8+  21)
                                                                   --26M3D9M3D6M6D8M2D21M
case_matchSize2 = fromRight (matchSize True =<< readSamLine simple) @?= 69
case_matchSize3 = fromRight (matchSize True =<< readSamLine refinsert) @?= 46

case_cigarOK = fixCigar "9M" 9 @?= Right "9M"
case_cigarH = fixCigar "4H5M" 9 @?= Right "4S5M"
case_cigarH2 = fixCigar "4H5M2H" 11 @?= Right "4S5M2S"
case_cigarH3 = fixCigar "5M1I40M54H" 46 @?= Right "5M1I40M54H"
case_cigarH4 = fixCigar "5M1I40M54H" 100 @?= Right "5M1I40M54S"
