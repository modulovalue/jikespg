%OPTIONS LA=2
%OPTIONS GENERATEPARSER=java
%OPTIONS act
%OPTIONS ACTFILENAME=bnfact.java
%OPTIONS HACTFILENAME=bnfhdr.java
%OPTIONS TABLE=space
%OPTIONS FILEPREFIX=bnf
%OPTIONS prefix=TK_
%OPTIONS nogotodefault
%OPTIONS names=max
%OPTIONS errormaps

Options in effect:
    ACTION  ACTFILE-NAME=bnfact.java  BLOCKB=/.  BLOCKE=./  BYTE  CONFLICTS  
    DEFAULT=5  NODEBUG  DEFERRED  NOEDIT  ERROR-MAPS  ESCAPE=%  FILE-PREFIX=bnf
    NOFIRST  NOFOLLOW  GENERATE-PARSER=JAVA  NOGOTO-DEFAULT  
    HACTFILE-NAME=bnfhdr.java  HBLOCKB=/:  HBLOCKE=:/  LALR=2  NOLIST  
    MAX-DISTANCE=30  MIN-DISTANCE=3  NAMES=MAXIMUM  NONT-CHECK  ORMARK=|  
    OUTPUT-SIZE=5000  PREFIX=TK_  READ-REDUCE  NOSCOPES  NOSHIFT-DEFAULT  
    NOSINGLE-PRODUCTIONS  STACK-SIZE=128  NOSTATES  SUFFIX=  TABLE=SPACE  
    TRACE=CONFLICTS  NOVERBOSE  WARNINGS  NOXREF  



This grammar is LALR(2).

Number of Terminals: 5
Number of Nonterminals: 4
Number of Productions: 8
Number of Items: 20
Number of States: 8
Number of look-ahead states: 2
Number of Shift actions: 3
Number of Goto actions: 4
Number of Shift/Reduce actions: 6
Number of Goto/Reduce actions: 1
Number of Reduce actions: 17
Number of Shift-Reduce conflicts: 0
Number of Reduce-Reduce conflicts: 0
Length of base Action Table: 13
Number of entries in base Action Table: 13
Percentage of increase: 0.0%
Storage required for base Tables: 26 Bytes, 1K
Storage required for Rules: 14 Bytes
Number of unique terminal states: 9
Number of Shift actions saved by merging: 0
Number of Reduce actions saved by merging: 3
Number of Reduce saved by default: 13
Length of Terminal Check Table: 26
Length of Terminal Action Table: 21
Number of entries in Terminal Action Table: 19
Percentage of increase: 10.5%
Storage required for Terminal Tables: 68 Bytes, 1K
Total storage required for Tables: 94 Bytes, 1K


Actions in Compressed Tables:
     Number of Shifts: 3
     Number of Shift/Reduces: 6
     Number of Look-Ahead Shifts: 2
     Number of Gotos: 4
     Number of Goto/Reduces: 1
     Number of Reduces: 1
     Number of Defaults: 7

Error maps storage:
    Storage required for ACTION_SYMBOLS_BASE map: 16 Bytes
    Storage required for ACTION_SYMBOLS_RANGE map: 6 Bytes
    Storage required for NACTION_SYMBOLS_BASE map: 16 Bytes
    Storage required for NACTION_SYMBOLS_RANGE map: 12 Bytes
    Storage required for TERMINAL_INDEX map: 5 Bytes
    Storage required for NON_TERMINAL_INDEX map: 5 Bytes
    Storage required for STRING_BUFFER map: 106 Bytes

***Warning: Base Check vector contains value > 127. 16-bit words used.
