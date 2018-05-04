Reads were artificially produced from the reference in order to match the following illustration:

    Name      Representation                            Coords
    reference ========================================   1:400
        geneV     ******                                40:100
        geneW            **                            110:130
        geneX               *******                    140:200
        geneY                         ********         230:310
        geneZ                              ********    280:360

                                            # of regions / feature size
    feature_A     ******    *******                    2 / 120
    feature_B     ****** **                            2 /  80
    feature_C                         ********         1 /  80
    feature_D     ******                   ********    2 / 140

        read0   +++++                                   20:70
        read1      +++++                                50:100
        read2        +++++                              70:120
        read3          +++++                            90:140
        read4               +++++                      140:190
        read5                      +++++               210:260
        read6                         +++++            240:290
        read7                             +++++        280:330
        read8                                   +++++  340:390


Expected feature counts per-read are:

           uniqueOnly  allOne  oneOverN  distOne
    read0           -     ABD       ABD       AD
    read1           -     ABD       ABD       AD
    read2           -     ABD       ABD       AD
    read3           -     ABD       ABD       AD
    read4           A       A         A        A
    read5           C       C         C        C
    read6           -      CD        CD       CD
    read7           -      CD        CD       CD
    read8           D       D         D        D

results the following feature counts:

           uniqueOnly  allOne  oneOverN  distOne
       -1           0       0         0        0
feature_A           1       5      2.33     3.15
feature_B           0       4      1.33        0
feature_C           1       3         2     2.27
feature_D           1       7      3.33     3.57
