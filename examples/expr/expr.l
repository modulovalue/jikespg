%OPTIONS GENERATEPARSER=java
%OPTIONS ACTFILENAME=expract.java
%OPTIONS HACTFILENAME=exprhdr.java
%OPTIONS TABLE=space
%OPTIONS FILEPREFIX=expr
%OPTIONS PREFIX=TK_
%OPTIONS NOGOTODEFAULT
%OPTIONS NAME=max
%OPTIONS ERRORMAPS

Options in effect:
    ACTFILENAME=expract.java  BLOCKB=/.  BLOCKE=./  BYTE  CONFLICTS  DEFAULT=5
    NODEBUG  ERROR-MAPS  ESCAPE=%  FILE-PREFIX=expr  NOFIRST  NOFOLLOW  
    GENERATE-PARSER=JAVA  NOGOTODEFAULT  HACTFILENAME=exprhdr.java  HBLOCKB=/:
    HBLOCKE=:/  LALR=1  NOLIST  MIN-DISTANCE=3  MAX-DISTANCE=30  NAMES=MAXIMUM
    NONTCHECK  ORMARK=|  PREFIX=TK_  READREDUCE  NOSCOPES  NOSHIFT-DEFAULT  
    NOSINGLE-PRODUCTIONS  STACK-SIZE=128  NOSTATES  SUFFIX=  TABLE=SPACE  
    TRACE=CONFLICTS  



This grammar is LALR(1).

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
Number of unique terminal states: 9
Number of Shift actions saved by merging: 12
Number of Reduce actions saved by merging: 0
Number of Reduce saved by default: 14
Reallocating storage for SPACE table, adding 10 entries
Length of Terminal Check Table: 41
Length of Terminal Action Table: 37
Number of entries in Terminal Action Table: 28
Percentage of increase: 32.1%


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
