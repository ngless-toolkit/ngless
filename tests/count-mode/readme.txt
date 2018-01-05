Reads were artificially produced from the reference in order to match the following illustration:

    Name      Representation                      Coords
    
    reference ========================================   1:400
    feature_A     ******                                40:100
    feature_B         ************                      80:200
    feature_C              **************              130:270
    feature_D                             *****        280:330
    
        read0   +++++                                   20:70
        read1      +++++                                50:100
        read2        +++++                              70:120
        read3          +++++                            90:140
        read4               +++++                      140:190
        read5                      +++++               210:260
        read6                         +++++            240:290
        read7                             +++++        280:330
        read8                                   +++++  340:390

read0 partially overlaps feature_A
read1 is contained in feature_A and partially overlaps feature_B
read2 partially overlaps feature_A and feature_B (overlapping features)
read3 is contained in feature_B and partially overlaps feature_A and feature_C
read4 is contained in both feature_B and feature_C
read5 is contained in feature_C
read6 partially overlaps with feature_C and feature_D (non-overlapping features)
read7 exactly overlaps feature_D
read8 doesn't overlap any feature

Expected feature counts per-read are:

            union  non-empty  strict
    read0       A          A       -
    read1      AB          A       A
    read2      AB          -       -
    read3     ABC          B       B
    read4      BC         BC      BC
    read5       C          C       C
    read6      CD          -       -
    read7       D          D       D
    read8       -          -       -

resulting in:

            union  non-empty  strict
       -1       1          3       4
feature_A       4          2       1
feature_B       4          2       2
feature_C       4          2       2
feature_D       2          1       1
