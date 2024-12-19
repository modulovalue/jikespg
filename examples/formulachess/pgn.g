-- Source: https://github.com/othomann/formulachess/blob/77ae9b47a6bbcd60a5f8681f4b54862cbeba96aa/bundles/org.formulachess.chess.utils/utils/pgn.g
--main options
%options ACTFILENAME=JavaAction.java, GENERATEPARSER=java,
%options FILEPREFIX=java, ESCAPE=$, PREFIX=TokenName,
%options NOGOTODEFAULT, SINGLEPRODUCTIONS, LALR=10 , TABLE=TIME ,

--grammar understanding options
%options first follow
%options TRACE=FULL ,

--Useful macros helping reading/writing semantic actions
$Define
$putCase
/.	    case $rule_number : if (DEBUG) System.out.println("$rule_text");  //$NON-NLS-1$
			./

$break
/.

			break;./

-- here it starts really ------------------------------------------
$Terminals

	IntegerLiteral
	StringLiteral

	FileName
	RankName
	PieceIdentification
	Start_Tag_Section
	Start_nag

	START_VARIATION
	END_VARIATION
	DOT

	END_TAG_SECTION
	WHITE_VICTORY
	BLACK_VICTORY
	DRAW
	UNKNOWN
	CAPTURE
	CHECK
	CHECKMATE
	CASTLE_KING_SIDE
	CASTLE_QUEEN_SIDE
	PROMOTE
	EXCELLENT_MOVE
	VERY_BAD_MOVE
	BAD_MOVE
	GOOD_MOVE
	INTERESTING_MOVE
	SUSPICIOUS_MOVE
	GREATER_THAN
	EN_PASSANT
	WHITESPACE

$Start
	Goal
$Rules

/. 	// This method is part of an automatic generation : do NOT edit-modify
	protected void consumeRule(int act) {
	  switch ( act ) {
./

Goal ::= GREATER_THAN PGN-database
 /.$putCase consumeGoal(); $break ./

PGN-database ::= PGN-database PGN-game
 /.$putCase consumePGNDatabase(); $break ./
PGN-database ::= $empty
/.$putCase consumeEmptyPGNDatabase(); $break ./

PGN-game ::= tag-section movetext-section space
/.$putCase consumePGNGame(); $break ./

tag-section ::= tag-section tag-pair space
/.$putCase consumeTagSection(); $break ./
tag-section ::= $empty
/.$putCase consumeEmptyTagSection(); $break ./

tag-pair ::= Start_Tag_Section space tag-value space END_TAG_SECTION
/.$putCase consumeTagPair(); $break ./

tag-value -> StringLiteral

movetext-section ::= element-sequence game-termination
/.$putCase consumeMoveTextSection(); $break ./

movetext-section ::= element-sequence
/.$putCase consumeMoveTextSectionWithoutGameTermination(); $break ./

element-sequence ::= element-sequence element
/.$putCase consumeElementSequence(); $break ./
element-sequence ::= element-sequence recursive-variation
/.$putCase consumeElementSequenceWithRecursiveVariation(); $break ./
element-sequence ::= $empty
/.$putCase consumeEmptyElementSequence(); $break ./

element ::= move-number-indication WhiteMove space
/.$putCase consumeElementSingleMove(); $break ./
element ::= move-number-indication WhiteMove WHITESPACE BlackMoveFollowingWhiteMove space
/.$putCase consumeElementTwoMoves(); $break ./
element ::= move-number-indication BlackMove space
/.$putCase consumeElementBlackMove(); $break ./

WhiteMove ::= InnerSANMove numeric-annotation-glyph
/.$putCase consumeWhiteMove(); $break ./
WhiteMove ::= InnerSANMove CHECK numeric-annotation-glyph
/.$putCase consumeWhiteMoveWithCheck(); $break ./
WhiteMove ::= InnerSANMove CHECKMATE numeric-annotation-glyph
/.$putCase consumeWhiteMoveWithCheckMate(); $break ./
BlackMove ::= BlackDots InnerSANMove numeric-annotation-glyph
/.$putCase consumeBlackMove(); $break ./
BlackMove ::= BlackDots InnerSANMove CHECK numeric-annotation-glyph
/.$putCase consumeBlackMoveWithCheck(); $break ./
BlackMove ::= BlackDots InnerSANMove CHECKMATE numeric-annotation-glyph
/.$putCase consumeBlackMoveWithCheckMate(); $break ./

BlackDots -> DOT DOT space

BlackMoveFollowingWhiteMove ::= WhiteMove
/.$putCase consumeBlackMoveFollowingWhiteMove(); $break ./

InnerSANMove ::= FileName RankName
/.$putCase consumePawnMove(); $break ./
InnerSANMove ::= FileName RankName PROMOTE PieceIdentification
/.$putCase consumePawnMoveWithPromotion(); $break ./
InnerSANMove ::= FileName RankName PieceIdentification
/.$putCase consumePawnMoveWithPromotion(); $break ./
InnerSANMove ::= FileName CAPTURE FileName RankName
/.$putCase consumePawnMoveWithCapture(); $break ./
InnerSANMove ::= FileName CAPTURE FileName RankName EN_PASSANT
/.$putCase consumePawnMoveWithCapture(); $break ./
InnerSANMove ::= FileName CAPTURE FileName RankName PieceIdentification
/.$putCase consumePawnMoveWithCaptureAndPromotion(); $break ./
InnerSANMove ::= FileName CAPTURE FileName RankName PROMOTE PieceIdentification
/.$putCase consumePawnMoveWithCaptureAndPromotion(); $break ./
InnerSANMove ::= PieceIdentification FileName RankName
/.$putCase consumePieceMove(); $break ./
InnerSANMove ::= PieceIdentification FileName FileName RankName
/.$putCase consumePieceMoveWithFileNameAmbiguity(); $break ./
InnerSANMove ::= PieceIdentification RankName FileName RankName
/.$putCase consumePieceMoveWithRankNameAmbiguity(); $break ./
InnerSANMove ::= PieceIdentification CAPTURE FileName RankName
/.$putCase consumePieceMoveWithCapture(); $break ./
InnerSANMove ::= PieceIdentification FileName CAPTURE FileName RankName
/.$putCase consumePieceMoveWithCaptureAndFileNameAmbiguity(); $break ./
InnerSANMove ::= PieceIdentification RankName CAPTURE FileName RankName
/.$putCase consumePieceMoveWithCaptureAndRankNameAmbiguity(); $break ./
InnerSANMove ::= PieceIdentification FileName RankName CAPTURE FileName RankName
/.$putCase consumePieceMoveWithCaptureAndDoubleAmbiguity(); $break ./
InnerSANMove ::= CASTLE_KING_SIDE
/.$putCase consumeCastleKingSide(); $break ./
InnerSANMove ::= CASTLE_QUEEN_SIDE
/.$putCase consumeCastleQueenSide(); $break ./

numeric-annotation-glyph ::= numeric-annotation-glyph Start_nag
/.$putCase consumeNAG(); $break ./
numeric-annotation-glyph ::= EXCELLENT_MOVE
/.$putCase consumeExcellentMoveNAG(); $break ./
numeric-annotation-glyph ::= VERY_BAD_MOVE
/.$putCase consumeVeryBadMoveNAG(); $break ./
numeric-annotation-glyph ::= BAD_MOVE
/.$putCase consumeBadMoveNAG(); $break ./
numeric-annotation-glyph ::= GOOD_MOVE
/.$putCase consumeGoodMoveMoveNAG(); $break ./
numeric-annotation-glyph ::= INTERESTING_MOVE
/.$putCase consumeInterestingMoveNAG(); $break ./
numeric-annotation-glyph ::= SUSPICIOUS_MOVE
/.$putCase consumeSuspiciousMoveNAG(); $break ./
numeric-annotation-glyph ::= $empty
/.$putCase consumeEmptyNAG(); $break ./

move-number-indication ::= IntegerLiteral DOT space
/.$putCase consumeMoveIndication(); $break ./

recursive-variation ::= START_VARIATION space element-sequence END_VARIATION space
/.$putCase consumeRecursiveVariation(); $break ./

space ::= WHITESPACE
space ::= $empty

game-termination ::= WHITE_VICTORY
/.$putCase consumeWhiteVictory(); $break ./
game-termination ::= BLACK_VICTORY
/.$putCase consumeBlackVictory(); $break ./
game-termination ::= DRAW
/.$putCase consumeDraw(); $break ./
game-termination ::= UNKNOWN
/.$putCase consumeUnknownResult(); $break ./

/.		}
	}./

---------------------------------------------------------------------------------------
$names

-- BodyMarker ::= '"class Identifier { ... MethodHeader "'

-- void ::= 'void'

START_VARIATION ::= '('
END_VARIATION ::= ')'
DOT ::= '.'
END_TAG_SECTION ::= ']'
WHITE_VICTORY ::= '1-0'
BLACK_VICTORY ::= '0-1'
DRAW ::= '1/2-1/2'
UNKNOWN ::= '*'
CAPTURE ::= 'x'
CHECK ::= '+'
CHECKMATE ::= '#'
CASTLE_KING_SIDE ::= 'O-O'
CASTLE_QUEEN_SIDE ::= 'O-O-O'
PROMOTE ::= '='
EXCELLENT_MOVE ::= '!!'
VERY_BAD_MOVE ::= '??'
BAD_MOVE ::= '?'
GOOD_MOVE ::= '!'
INTERESTING_MOVE ::= '!?'
SUSPICIOUS_MOVE ::= '?!'
GREATER_THAN ::= '>'

$end
-- need a carriage return after the $end