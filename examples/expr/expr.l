

 IBM Research Jikes Parser Generator    1.2 Sun Dec  8 02:39:48 2024       Page 1

%OPTIONS gp=java,act,tab,an=expract.java,hn=exprhdr.java,tab=space,fp=expr,prefix=TK_,
%OPTIONS nogoto-default,output-size=125,name=max,error-maps
***ERROR: "OUTPUT-SIZE" is an invalid option

Options in effect:
    ACTION  ACTFILE-NAME=expract.java  BLOCKB=/.  BLOCKE=./  BYTE  CONFLICTS  
    DEFAULT=5  NODEBUG  DEFERRED  NOEDIT  ERROR-MAPS  ESCAPE=%  
    FILE-PREFIX=expr  NOFIRST  NOFOLLOW  GENERATE-PARSER=JAVA  NOGOTO-DEFAULT
    HACTFILE-NAME=exprhdr.java  HBLOCKB=/:  HBLOCKE=:/  LALR=1  NOLIST  
    MAX-DISTANCE=30  MIN-DISTANCE=3  NAMES=MAXIMUM  NONT-CHECK  ORMARK=|  
    OUTPUT-SIZE=5000  PREFIX=TK_  READ-REDUCE  NOSCOPES  NOSHIFT-DEFAULT  
    NOSINGLE-PRODUCTIONS  STACK-SIZE=128  NOSTATES  SUFFIX=  TABLE=SPACE  
    TRACE=CONFLICTS  NOVERBOSE  WARNINGS  NOXREF  



This grammar is LALR(1).


 IBM Research Jikes Parser Generator    1.2 Sun Dec  8 02:39:48 2024       Page 2


Number of Terminals: 9
Number of Nonterminals: 4
Number of Productions: 12
Number of Items: 34
Number of States: 13
Number of Shift actions: 22
Number of Goto actions: 7
Number of Shift/Reduce actions: 8
Number of Goto/Reduce actions: 6
Number of Reduce actions: 15
Number of Shift-Reduce conflicts: 0
Number of Reduce-Reduce conflicts: 0
Length of base Action Table: 26
Number of entries in base Action Table: 26
Percentage of increase: 0.0%
Storage required for base Tables: 52 Bytes, 1K
Storage required for Rules: 22 Bytes
Number of unique terminal states: 9
Number of Shift actions saved by merging: 12
Number of Reduce actions saved by merging: 0
Number of Reduce saved by default: 14
Length of Terminal Check Table: 41
Length of Terminal Action Table: 37
Number of entries in Terminal Action Table: 28
Percentage of increase: 32.1%
Storage required for Terminal Tables: 115 Bytes, 1K
Total storage required for Tables: 167 Bytes, 1K


Actions in Compressed Tables:
     Number of Shifts: 14
     Number of Shift/Reduces: 4
     Number of Gotos: 7
     Number of Goto/Reduces: 6
     Number of Reduces: 1
     Number of Defaults: 5

Error maps storage:
    Storage required for ACTION_SYMBOLS_BASE map: 26 Bytes
    Storage required for ACTION_SYMBOLS_RANGE map: 16 Bytes
    Storage required for NACTION_SYMBOLS_BASE map: 26 Bytes
    Storage required for NACTION_SYMBOLS_RANGE map: 8 Bytes
    Storage required for TERMINAL_INDEX map: 9 Bytes
    Storage required for NON_TERMINAL_INDEX map: 5 Bytes
    Storage required for STRING_BUFFER map: 148 Bytes

***Warning: Base Check vector contains value > 127. 16-bit words used.
