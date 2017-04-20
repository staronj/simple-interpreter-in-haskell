{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGrammar where
import AbsGrammar
import LexGrammar
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn36 :: (Integer) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (Integer)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (Ident) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (Ident)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Boolean) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Boolean)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (Literal) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (Literal)
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (Expr) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (Expr)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (Expr) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (Expr)
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (Expr) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (Expr)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Expr) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Expr)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (Expr) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (Expr)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (Expr) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (Expr)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (Expr) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (Expr)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (Expr) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (Expr)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (SepExprList) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (SepExprList)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (SepExprList) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (SepExprList)
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (Expr) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (Expr)
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: (Expr) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> (Expr)
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (Expr) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (Expr)
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: (MarkExprList) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (MarkExprList)
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: (MarkExprList) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> (MarkExprList)
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: (Type) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> (Type)
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (MarkTypeList) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (MarkTypeList)
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: (MarkTypeList) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> (MarkTypeList)
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: (Pattern) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> (Pattern)
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: (MarkPatternList) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> (MarkPatternList)
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: (MarkPatternList) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> (MarkPatternList)
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: (FunDecl) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> (FunDecl)
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyIn62 :: (Parameter) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> (Parameter)
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyIn63 :: (SepParameterList) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> (SepParameterList)
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
happyIn64 :: (SepParameterList) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> (SepParameterList)
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
happyIn65 :: (IfElse) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> (IfElse)
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
happyIn66 :: (IfStmt) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> (IfStmt)
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
happyIn67 :: (Stmt) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> (Stmt)
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
happyIn68 :: (Block) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> (Block)
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
happyIn69 :: ([Stmt]) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> ([Stmt])
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
happyIn70 :: (Program) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> (Program)
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
happyIn71 :: ([FunDecl]) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> ([FunDecl])
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xf3\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x5a\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x3d\x05\x3d\x05\x3d\x05\xfe\xff\xfe\xff\xfe\xff\xbd\x01\xfe\xff\xfe\xff\xfe\xff\xbf\x01\xbe\x01\x6c\x00\xb5\x01\x00\x00\x00\x00\x00\x00\xb4\x01\x00\x00\xee\xff\xb0\x01\xb7\x01\x01\x00\xa9\x01\x00\x00\x00\x00\x2b\x00\x00\x00\x00\x00\xb1\x01\xb2\x01\x9b\x01\x00\x00\xa6\x00\x6e\x00\x00\x00\x07\x00\xba\x01\xc3\x00\x00\x00\x00\x00\xe7\x00\x00\x00\x99\x01\xc7\x00\x9e\x00\x96\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\xaf\x01\xad\x01\x92\x01\x92\x01\x9e\x00\xfe\xff\x98\x01\x9e\x00\x00\x00\x00\x00\x91\x01\x9e\x00\x91\x01\x9e\x00\x00\x00\xaa\x01\x00\x00\x3d\x00\xfe\xff\x00\x00\x90\x01\xae\x01\x8c\x01\x8c\x01\x8c\x01\x00\x00\x3a\x00\xa8\x01\x79\x01\x79\x01\x00\x00\x2d\x00\x7a\x00\x3d\x05\x3d\x05\x00\x00\x00\x00\x97\x01\x78\x01\x78\x01\x00\x00\x04\x00\x00\x00\x00\x00\x96\x01\x74\x01\x74\x01\x74\x01\x74\x01\x00\x00\xf9\xff\x94\x01\x70\x01\x48\x00\x70\x01\x10\x00\xfd\xff\x70\x01\x70\x01\x70\x01\x70\x01\x00\x00\x70\x01\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x72\x01\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x3d\x05\x88\x01\x8f\x01\x00\x00\x3d\x05\x3d\x05\xfe\xff\xfe\xff\xfe\xff\x00\x00\x8a\x01\xfe\xff\x3d\x05\x6d\x01\x6d\x01\x6d\x01\x00\x00\x3b\x00\x6d\x01\x62\x01\x7a\x01\x00\x00\x00\x00\x71\x01\x8f\x00\x68\x01\x00\x00\x00\x00\x45\x00\x76\x01\x00\x00\x9e\x00\x00\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x9e\x00\x00\x00\x9e\x00\x42\x00\x00\x00\x00\x00\xf0\xff\x00\x00\x75\x01\x00\x00\x00\x00\x00\x00\xa6\x00\xa6\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x01\x53\x01\xfe\xff\x9e\x00\x5f\x01\x3d\x05\x9e\x00\x00\x00\x00\x00\x5f\x01\x00\x00\x00\x00\x00\x00\x6f\x01\x00\x00\x6e\x01\x00\x00\x00\x00\x00\x00\x4f\x01\x6c\x01\x00\x00\x6b\x01\x00\x00\x67\x01\x49\x01\x00\x00\x00\x00\x00\x00\x00\x00\x6e\x00\x6e\x00\x00\x00\x48\x01\x38\x01\x4a\x01\x47\x01\x34\x00\x52\x01\x42\x01\x41\x01\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x9e\x00\x9e\x00\x00\x00\x00\x00\x00\x00\x45\x01\x30\x01\x00\x00\x3d\x05\x30\x01\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x0f\x00\x19\x04\x3a\x04\x5b\x04\xdf\x04\x3c\x05\x6f\x05\xfc\x05\xe0\x00\x6c\x02\x4b\x02\x9d\x04\xf8\x04\xb3\x05\x2a\x02\x09\x02\x3d\x01\xd1\x00\x65\x00\xd2\x00\xfe\x00\xac\x00\x25\x01\xdd\x00\x82\x01\x9a\x00\x37\x01\x31\x01\x43\x01\x1d\x01\x1b\x01\xab\x00\x16\x01\x00\x00\x00\x00\x21\x01\x00\x00\x21\x01\x22\x01\x00\x00\x17\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x05\xec\x05\xe8\x01\xd8\x05\xca\x05\xc7\x01\x00\x00\x00\x00\x36\x01\x35\x01\xf8\x03\xa4\x00\x15\x01\xd7\x03\x00\x00\x00\x00\x00\x00\xb6\x03\x00\x00\x95\x03\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x00\x33\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x01\x40\x00\x1c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x05\x4d\x05\xa2\x05\x91\x05\x80\x05\x2e\x01\x74\x03\xa6\x01\x53\x03\x85\x01\x32\x03\xf8\xff\x00\x00\x00\x00\x00\x00\x06\x01\x03\x01\x74\x00\x44\x00\x92\x00\x00\x00\x00\x00\xa2\x00\xde\x00\xf2\x00\xd5\x00\xbf\x00\x00\x00\x00\x00\xbd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x05\x00\x00\x2b\x05\x1a\x05\x09\x05\xbe\x04\x7c\x04\x11\x03\x00\x00\x64\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\x00\xb7\x00\xfd\x00\xf0\x02\x00\x00\xae\x00\xcf\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8a\x00\x00\x00\x00\x00\x87\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x67\x00\x00\x00\xae\x02\x8d\x02\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x00\x00\x5b\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb5\xff\x00\x00\x00\x00\x00\x00\x00\x00\xad\xff\x00\x00\x00\x00\xa2\xff\x00\x00\x00\x00\x99\xff\x00\x00\x00\x00\x00\x00\x91\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\xff\x78\xff\x78\xff\x00\x00\xde\xff\x00\x00\x00\x00\x79\xff\x00\x00\x00\x00\x7b\xff\xda\xff\xc0\xff\xdb\xff\xc1\xff\x00\x00\xd7\xff\xd5\xff\xb0\xff\xaf\xff\xcc\xff\xae\xff\xc2\xff\xd3\xff\xcf\xff\xc8\xff\x8a\xff\x85\xff\x86\xff\x00\x00\x7e\xff\x00\x00\x00\x00\xad\xff\x00\x00\x00\x00\xad\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\xdc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x9d\xff\x00\x00\x8e\xff\x00\x00\x99\xff\x9b\xff\x00\x00\x90\xff\x00\x00\x00\x00\x00\x00\x96\xff\x00\x00\x00\x00\x00\x00\x00\x00\x9f\xff\x00\x00\x00\x00\xa2\xff\x00\x00\xa8\xff\xa7\xff\x00\x00\x00\x00\x00\x00\xaa\xff\x00\x00\xbc\xff\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\xff\x00\x00\xb4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xac\xff\x00\x00\xa1\xff\x00\x00\x00\x00\xa6\xff\x00\x00\x00\x00\x98\xff\x00\x00\x00\x00\x9c\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\xff\x00\x00\x00\x00\x00\x00\x00\x00\x87\xff\x88\xff\xda\xff\x00\x00\x00\x00\xc7\xff\xc6\xff\x00\x00\x00\x00\xc4\xff\x00\x00\xc5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x89\xff\xb5\xff\x00\x00\x7a\xff\x77\xff\x00\x00\x7d\xff\x00\x00\xd8\xff\xd6\xff\xd4\xff\xd2\xff\xd0\xff\xd1\xff\xc3\xff\xb7\xff\xb6\xff\xba\xff\x00\x00\x00\x00\x91\xff\x00\x00\x8b\xff\x00\x00\x00\x00\x83\xff\x8b\xff\x00\x00\x92\xff\x8d\xff\x9a\xff\x8f\xff\x95\xff\x97\xff\x9e\xff\xa5\xff\xa3\xff\x00\x00\xa0\xff\xa9\xff\xab\xff\xb1\xff\xb3\xff\x00\x00\xbd\xff\xca\xff\xcb\xff\xc9\xff\xcd\xff\xce\xff\xbe\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\xff\x7c\xff\xb9\xff\xb8\xff\x00\x00\x82\xff\x00\x00\x00\x00\x7f\xff\x8c\xff\xa4\xff\x00\x00\x00\x00\x94\xff\x00\x00\x00\x00\x81\xff\x80\xff\x93\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x11\x00\x01\x00\x0a\x00\x06\x00\x04\x00\x09\x00\x06\x00\x0b\x00\x08\x00\x1c\x00\x13\x00\x0b\x00\x15\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x03\x00\x0d\x00\x17\x00\x15\x00\x27\x00\x08\x00\x2b\x00\x19\x00\x1a\x00\x15\x00\x1c\x00\x1d\x00\x0f\x00\x1f\x00\x23\x00\x21\x00\x22\x00\x2b\x00\x24\x00\x25\x00\x29\x00\x2b\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x02\x00\x03\x00\x2b\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0a\x00\x0d\x00\x0e\x00\x0f\x00\x2b\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x0e\x00\x01\x00\x0a\x00\x01\x00\x04\x00\x0a\x00\x06\x00\x20\x00\x08\x00\x10\x00\x07\x00\x0b\x00\x13\x00\x0a\x00\x25\x00\x26\x00\x27\x00\x13\x00\x14\x00\x0d\x00\x2b\x00\x15\x00\x2b\x00\x25\x00\x16\x00\x19\x00\x1a\x00\x15\x00\x1c\x00\x1d\x00\x06\x00\x1f\x00\x0c\x00\x21\x00\x22\x00\x2b\x00\x24\x00\x25\x00\x2b\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x01\x00\x13\x00\x15\x00\x04\x00\x03\x00\x06\x00\x2b\x00\x08\x00\x01\x00\x08\x00\x0b\x00\x13\x00\x1f\x00\x15\x00\x25\x00\x20\x00\x0f\x00\x04\x00\x25\x00\x06\x00\x15\x00\x28\x00\x29\x00\x2a\x00\x19\x00\x1a\x00\x20\x00\x1c\x00\x1d\x00\x16\x00\x1f\x00\x18\x00\x21\x00\x22\x00\x15\x00\x24\x00\x25\x00\x18\x00\x01\x00\x28\x00\x29\x00\x2a\x00\x01\x00\x1e\x00\x0a\x00\x04\x00\x01\x00\x06\x00\x23\x00\x08\x00\x01\x00\x11\x00\x0b\x00\x04\x00\x01\x00\x06\x00\x01\x00\x08\x00\x20\x00\x16\x00\x0b\x00\x20\x00\x15\x00\x1a\x00\x01\x00\x1c\x00\x09\x00\x16\x00\x0b\x00\x00\x00\x15\x00\x1a\x00\x1f\x00\x1c\x00\x00\x00\x16\x00\x23\x00\x16\x00\x25\x00\x1a\x00\x1f\x00\x28\x00\x29\x00\x2a\x00\x13\x00\x16\x00\x25\x00\x18\x00\x02\x00\x28\x00\x29\x00\x2a\x00\x02\x00\x03\x00\x00\x00\x05\x00\x22\x00\x23\x00\x08\x00\x09\x00\x01\x00\x0b\x00\x01\x00\x0d\x00\x12\x00\x0f\x00\x14\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x20\x00\x01\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x13\x00\x14\x00\x16\x00\x17\x00\x16\x00\x02\x00\x03\x00\x0b\x00\x05\x00\x26\x00\x27\x00\x08\x00\x09\x00\x13\x00\x0b\x00\x16\x00\x0d\x00\x20\x00\x0f\x00\x1a\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x1d\x00\x01\x00\x01\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x26\x00\x27\x00\x0e\x00\x0f\x00\x10\x00\x20\x00\x16\x00\x16\x00\x17\x00\x13\x00\x1a\x00\x1b\x00\x13\x00\x19\x00\x28\x00\x29\x00\x2a\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x13\x00\x0e\x00\x0f\x00\x10\x00\x13\x00\x01\x00\x20\x00\x01\x00\x01\x00\x21\x00\x23\x00\x19\x00\x19\x00\x21\x00\x20\x00\x19\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x1e\x00\x13\x00\x0e\x00\x0f\x00\x10\x00\x1d\x00\x25\x00\x11\x00\x16\x00\x16\x00\x07\x00\x13\x00\x11\x00\x19\x00\x25\x00\x16\x00\x16\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0a\x00\x0e\x00\x0f\x00\x10\x00\x0a\x00\x0a\x00\x28\x00\x0a\x00\x0a\x00\x1b\x00\x28\x00\x07\x00\x07\x00\x16\x00\x0e\x00\x06\x00\x1d\x00\x20\x00\x01\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x07\x00\x25\x00\x0e\x00\x0f\x00\x10\x00\x07\x00\x12\x00\x16\x00\x11\x00\x28\x00\x2b\x00\x1a\x00\x1b\x00\x0a\x00\x2b\x00\x0a\x00\x0a\x00\x1d\x00\x2b\x00\x2b\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0a\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x2b\x00\x0a\x00\x29\x00\x10\x00\x29\x00\x2b\x00\x25\x00\x11\x00\x05\x00\x11\x00\x26\x00\x11\x00\x1d\x00\x2b\x00\x13\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x1c\x00\x2b\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x1c\x00\x25\x00\x2b\x00\x28\x00\x1f\x00\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\x0a\x00\x0b\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\x0a\x00\x0b\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\x0a\x00\x0b\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x04\x00\xff\xff\x06\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x15\x00\xff\xff\xff\xff\x18\x00\x09\x00\x0a\x00\x0b\x00\x1d\x00\xff\xff\x1e\x00\x20\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x0a\x00\x0b\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\x0a\x00\x0b\x00\x20\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x1d\x00\x0a\x00\x0b\x00\x20\x00\xff\xff\x0a\x00\x0b\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\xb5\x00\x3e\x00\x8b\x00\x56\x00\x3f\x00\x83\x00\x40\x00\x84\x00\x41\x00\x46\x00\x61\x00\x42\x00\xda\x00\x8d\x00\x29\x00\x80\x00\x2b\x00\x81\x00\x85\x00\x88\x00\x57\x00\x43\x00\xf1\x00\x86\x00\xff\xff\x44\x00\x45\x00\x89\x00\x46\x00\x47\x00\x87\x00\x48\x00\x58\x00\x49\x00\x4a\x00\xff\xff\x4b\x00\x29\x00\x4c\x00\xff\xff\x23\x00\x4c\x00\x4d\x00\xff\xff\xc0\xff\xc0\xff\xff\xff\xc0\xff\xb6\x00\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\x93\x00\xc0\xff\xc0\xff\xc0\xff\xff\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xf6\x00\x3e\x00\x95\x00\x51\x00\x3f\x00\x99\x00\x40\x00\x01\x01\x41\x00\xcc\x00\xc5\x00\x42\x00\xcd\x00\x8c\x00\xc0\xff\xc0\xff\xc0\xff\x68\x00\x8f\x00\x88\x00\xc0\xff\x43\x00\xff\xff\x29\x00\xd4\x00\x44\x00\x45\x00\x89\x00\x46\x00\x47\x00\x40\x00\x48\x00\xfe\x00\x49\x00\x4a\x00\xff\xff\x4b\x00\x29\x00\xff\xff\xbb\x00\x23\x00\x4c\x00\x4d\x00\x3e\x00\xfe\x00\x43\x00\x3f\x00\x85\x00\x40\x00\xff\xff\x41\x00\x51\x00\x86\x00\x42\x00\x61\x00\x51\x00\x62\x00\x29\x00\xff\x00\x87\x00\x64\x00\x29\x00\x65\x00\x43\x00\x23\x00\x4c\x00\x4d\x00\x44\x00\x45\x00\xfc\x00\x46\x00\x47\x00\x5c\x00\x48\x00\xd5\x00\x49\x00\x4a\x00\x66\x00\x4b\x00\x29\x00\x67\x00\x51\x00\x23\x00\x4c\x00\x4d\x00\x3e\x00\x68\x00\x8c\x00\x3f\x00\x51\x00\x40\x00\x92\x00\x41\x00\x3e\x00\xc7\x00\x42\x00\x3f\x00\x51\x00\x40\x00\x51\x00\x41\x00\xf4\x00\x52\x00\x42\x00\xf8\x00\x43\x00\x53\x00\x51\x00\xd3\x00\x83\x00\x52\x00\x84\x00\xe7\x00\x43\x00\x53\x00\x51\x00\x54\x00\xed\x00\x52\x00\xad\x00\x9e\x00\x29\x00\xd1\x00\x51\x00\x23\x00\x4c\x00\x4d\x00\xea\x00\x5c\x00\x29\x00\x5d\x00\xaf\x00\x23\x00\x4c\x00\x4d\x00\xbb\xff\xbb\xff\xee\x00\xbb\xff\x24\x00\x25\x00\x7e\xff\xbb\xff\x51\x00\x7e\xff\x51\x00\xbb\xff\xb0\x00\xbb\xff\xb1\x00\xbb\xff\xbb\xff\xbb\xff\xbb\xff\x7e\xff\xca\x00\x51\x00\xcd\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x68\x00\x69\x00\x5e\x00\x97\x00\x60\x00\xbc\xff\xbc\xff\x78\x00\xbc\xff\xbb\xff\x7e\xff\x85\xff\xbc\xff\xd0\x00\x85\xff\x52\x00\xbc\xff\xce\x00\xbc\xff\x5a\x00\xbc\xff\xbc\xff\xbc\xff\xbc\xff\x85\xff\x6d\x00\x51\x00\x51\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xb9\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xbc\xff\x85\xff\x35\x00\x36\x00\x37\x00\xcf\x00\x52\x00\x5e\x00\x5f\x00\xd6\x00\x58\x00\xec\x00\xd7\x00\x38\x00\x23\x00\x4c\x00\x4d\x00\x39\x00\x3a\x00\xb7\x00\x3c\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xe0\x00\x8e\x00\x35\x00\x36\x00\x37\x00\x90\x00\x96\x00\x9d\x00\xa0\x00\xa1\x00\xb6\x00\x23\x00\xb8\x00\x38\x00\x26\x00\x27\x00\x5b\x00\x39\x00\x3a\x00\xb7\x00\x3c\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x4d\x00\x6a\x00\x35\x00\x36\x00\x37\x00\x4f\x00\x29\x00\x01\x01\xf2\x00\xf3\x00\xf4\x00\xf7\x00\xf8\x00\x38\x00\x29\x00\xfa\x00\xe7\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x76\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xbb\x00\x8b\x00\x35\x00\x36\x00\x37\x00\x8d\x00\x93\x00\x23\x00\x95\x00\x99\x00\xe9\x00\x23\x00\xf0\x00\xc4\x00\xc6\x00\xc8\x00\xc9\x00\x6d\x00\xca\x00\x51\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x6b\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xd3\x00\x29\x00\x35\x00\x36\x00\x37\x00\xd9\x00\xdc\x00\x52\x00\xda\x00\x23\x00\xff\xff\x58\x00\x59\x00\x8a\x00\xff\xff\x8c\x00\x8e\x00\x6d\x00\xff\xff\xff\xff\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x74\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x94\x00\xde\x00\x35\x00\x36\x00\x37\x00\xff\xff\x96\x00\x4c\x00\x9a\x00\x4c\x00\xff\xff\x29\x00\xa3\x00\xb2\x00\xa4\x00\xb3\x00\xb5\x00\x6d\x00\xff\xff\xb4\x00\x6e\x00\xa4\x00\x2a\x00\x2b\x00\x2c\x00\xa5\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x46\x00\xff\xff\x35\x00\x36\x00\x37\x00\xa6\x00\x46\x00\x29\x00\xff\xff\x23\x00\x4f\x00\x51\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xa9\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\xaa\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x6b\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x6c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x6f\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x74\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x75\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x76\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x77\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xfa\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xfb\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xe9\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xeb\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xbc\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xdb\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xdd\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xdf\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x9a\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x9b\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x9c\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x9f\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x7f\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x7e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x7d\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\xbd\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x73\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\xbe\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x72\x00\x37\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x00\x32\x00\x33\x00\x34\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x37\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x32\x00\x33\x00\x34\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x37\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc1\x00\x32\x00\x33\x00\x34\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x37\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x64\x00\x00\x00\x65\x00\x7b\x00\x32\x00\x33\x00\x34\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x37\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x66\x00\x00\x00\x00\x00\x67\x00\xe4\x00\x33\x00\x34\x00\x6d\x00\x00\x00\x68\x00\x6e\x00\x37\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe5\x00\x33\x00\x34\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x37\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7a\x00\x33\x00\x34\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x37\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\xe1\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\xe2\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\xe3\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x71\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xc2\x00\x34\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\xa7\x00\x34\x00\x00\x00\x00\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\xa8\x00\x34\x00\x6e\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x6d\x00\xab\x00\x34\x00\x6e\x00\x00\x00\xad\x00\x34\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x34\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (33, 136) [
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136)
	]

happy_n_terms = 44 :: Int
happy_n_nonterms = 36 :: Int

happyReduce_33 = happySpecReduce_1  0# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn36
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_34 = happySpecReduce_1  1# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn37
		 (Ident happy_var_1
	)}

happyReduce_35 = happySpecReduce_1  2# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Boolean happy_var_1)) -> 
	happyIn38
		 (Boolean (happy_var_1)
	)}

happyReduce_36 = happySpecReduce_1  3# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (AbsGrammar.LiteralBool happy_var_1
	)}

happyReduce_37 = happySpecReduce_1  3# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (AbsGrammar.LiteralI32 happy_var_1
	)}

happyReduce_38 = happySpecReduce_1  3# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (AbsGrammar.LiteralIdent happy_var_1
	)}

happyReduce_39 = happySpecReduce_3  4# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 (AbsGrammar.Assign happy_var_1 happy_var_3
	)}}

happyReduce_40 = happySpecReduce_1  4# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (happy_var_1
	)}

happyReduce_41 = happySpecReduce_3  5# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 (AbsGrammar.Or happy_var_1 happy_var_3
	)}}

happyReduce_42 = happySpecReduce_1  5# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_43 = happySpecReduce_3  6# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (AbsGrammar.And happy_var_1 happy_var_3
	)}}

happyReduce_44 = happySpecReduce_1  6# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (happy_var_1
	)}

happyReduce_45 = happySpecReduce_3  7# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (AbsGrammar.Equal happy_var_1 happy_var_3
	)}}

happyReduce_46 = happySpecReduce_3  7# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (AbsGrammar.NotEqual happy_var_1 happy_var_3
	)}}

happyReduce_47 = happySpecReduce_3  7# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (AbsGrammar.Less happy_var_1 happy_var_3
	)}}

happyReduce_48 = happySpecReduce_1  7# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (happy_var_1
	)}

happyReduce_49 = happySpecReduce_3  8# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (AbsGrammar.Add happy_var_1 happy_var_3
	)}}

happyReduce_50 = happySpecReduce_3  8# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (AbsGrammar.Subtract happy_var_1 happy_var_3
	)}}

happyReduce_51 = happySpecReduce_1  8# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (happy_var_1
	)}

happyReduce_52 = happySpecReduce_3  9# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (AbsGrammar.Multiply happy_var_1 happy_var_3
	)}}

happyReduce_53 = happySpecReduce_3  9# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (AbsGrammar.Divide happy_var_1 happy_var_3
	)}}

happyReduce_54 = happySpecReduce_3  9# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (AbsGrammar.Modulo happy_var_1 happy_var_3
	)}}

happyReduce_55 = happySpecReduce_1  9# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (happy_var_1
	)}

happyReduce_56 = happySpecReduce_2  10# happyReduction_56
happyReduction_56 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (AbsGrammar.Negate happy_var_2
	)}

happyReduce_57 = happySpecReduce_2  10# happyReduction_57
happyReduction_57 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (AbsGrammar.Dereference happy_var_2
	)}

happyReduce_58 = happySpecReduce_2  10# happyReduction_58
happyReduction_58 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (AbsGrammar.Not happy_var_2
	)}

happyReduce_59 = happySpecReduce_2  10# happyReduction_59
happyReduction_59 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (AbsGrammar.Borrow happy_var_2
	)}

happyReduce_60 = happySpecReduce_3  10# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 (AbsGrammar.MutableBorrow happy_var_3
	)}

happyReduce_61 = happySpecReduce_1  10# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (happy_var_1
	)}

happyReduce_62 = happySpecReduce_1  11# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (AbsGrammar.LiteralExpr happy_var_1
	)}

happyReduce_63 = happySpecReduce_1  11# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (AbsGrammar.ExprIdent happy_var_1
	)}

happyReduce_64 = happyReduce 4# 11# happyReduction_64
happyReduction_64 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (AbsGrammar.FunctionCall happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_65 = happyReduce 4# 11# happyReduction_65
happyReduction_65 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (AbsGrammar.ArrayLookup happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_66 = happySpecReduce_3  11# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (AbsGrammar.TupleLookup happy_var_1 happy_var_3
	)}}

happyReduce_67 = happySpecReduce_1  11# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut65 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (AbsGrammar.IfElseExpr happy_var_1
	)}

happyReduce_68 = happySpecReduce_1  11# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (AbsGrammar.BlockExpr happy_var_1
	)}

happyReduce_69 = happySpecReduce_3  11# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (AbsGrammar.ArrayElements happy_var_2
	)}

happyReduce_70 = happyReduce 5# 11# happyReduction_70
happyReduction_70 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_2 of { happy_var_2 -> 
	case happyOut36 happy_x_4 of { happy_var_4 -> 
	happyIn47
		 (AbsGrammar.ArrayRepeat happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_71 = happyReduce 5# 11# happyReduction_71
happyReduction_71 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut36 happy_x_2 of { happy_var_2 -> 
	case happyOut36 happy_x_4 of { happy_var_4 -> 
	happyIn47
		 (AbsGrammar.ArrayRange happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_72 = happySpecReduce_3  11# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (AbsGrammar.TupleConstruct happy_var_2
	)}

happyReduce_73 = happySpecReduce_3  11# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (happy_var_2
	)}

happyReduce_74 = happySpecReduce_0  12# happyReduction_74
happyReduction_74  =  happyIn48
		 (AbsGrammar.SepExprNil
	)

happyReduce_75 = happySpecReduce_1  12# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (AbsGrammar.SepExprOne happy_var_1
	)}

happyReduce_76 = happySpecReduce_3  12# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (AbsGrammar.SepExprMore happy_var_1 happy_var_3
	)}}

happyReduce_77 = happySpecReduce_1  13# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (AbsGrammar.SepExprHead happy_var_1
	)}

happyReduce_78 = happySpecReduce_3  13# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (AbsGrammar.SepExprTail happy_var_1 happy_var_3
	)}}

happyReduce_79 = happySpecReduce_1  14# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_80 = happySpecReduce_1  15# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (happy_var_1
	)}

happyReduce_81 = happySpecReduce_1  16# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (happy_var_1
	)}

happyReduce_82 = happySpecReduce_0  17# happyReduction_82
happyReduction_82  =  happyIn53
		 (AbsGrammar.MarkExprNil
	)

happyReduce_83 = happySpecReduce_2  17# happyReduction_83
happyReduction_83 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 (AbsGrammar.MarkExprOne happy_var_1
	)}

happyReduce_84 = happySpecReduce_3  17# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (AbsGrammar.MarkExprMore happy_var_1 happy_var_3
	)}}

happyReduce_85 = happySpecReduce_1  18# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 (AbsGrammar.MarkExprHead happy_var_1
	)}

happyReduce_86 = happySpecReduce_3  18# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 (AbsGrammar.MarkExprTail happy_var_1 happy_var_3
	)}}

happyReduce_87 = happySpecReduce_1  19# happyReduction_87
happyReduction_87 happy_x_1
	 =  happyIn55
		 (AbsGrammar.Bool
	)

happyReduce_88 = happySpecReduce_1  19# happyReduction_88
happyReduction_88 happy_x_1
	 =  happyIn55
		 (AbsGrammar.I32
	)

happyReduce_89 = happySpecReduce_2  19# happyReduction_89
happyReduction_89 happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_2 of { happy_var_2 -> 
	happyIn55
		 (AbsGrammar.Reference happy_var_2
	)}

happyReduce_90 = happySpecReduce_3  19# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn55
		 (AbsGrammar.MutableReference happy_var_3
	)}

happyReduce_91 = happyReduce 5# 19# happyReduction_91
happyReduction_91 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_2 of { happy_var_2 -> 
	case happyOut36 happy_x_4 of { happy_var_4 -> 
	happyIn55
		 (AbsGrammar.Array happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_92 = happySpecReduce_3  19# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_2 of { happy_var_2 -> 
	happyIn55
		 (AbsGrammar.Tuple happy_var_2
	)}

happyReduce_93 = happySpecReduce_0  20# happyReduction_93
happyReduction_93  =  happyIn56
		 (AbsGrammar.MarkTNil
	)

happyReduce_94 = happySpecReduce_2  20# happyReduction_94
happyReduction_94 happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (AbsGrammar.MarkTOne happy_var_1
	)}

happyReduce_95 = happySpecReduce_3  20# happyReduction_95
happyReduction_95 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut57 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (AbsGrammar.MarkTMore happy_var_1 happy_var_3
	)}}

happyReduce_96 = happySpecReduce_1  21# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (AbsGrammar.MarkTHead happy_var_1
	)}

happyReduce_97 = happySpecReduce_3  21# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn57
		 (AbsGrammar.MarkTTail happy_var_1 happy_var_3
	)}}

happyReduce_98 = happySpecReduce_1  22# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (AbsGrammar.PatternVariable happy_var_1
	)}

happyReduce_99 = happySpecReduce_2  22# happyReduction_99
happyReduction_99 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn58
		 (AbsGrammar.PatternMutableVariable happy_var_2
	)}

happyReduce_100 = happySpecReduce_1  22# happyReduction_100
happyReduction_100 happy_x_1
	 =  happyIn58
		 (AbsGrammar.PatternIgnore
	)

happyReduce_101 = happySpecReduce_3  22# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_2 of { happy_var_2 -> 
	happyIn58
		 (AbsGrammar.PatternTuple happy_var_2
	)}

happyReduce_102 = happySpecReduce_0  23# happyReduction_102
happyReduction_102  =  happyIn59
		 (AbsGrammar.MarkPatternNil
	)

happyReduce_103 = happySpecReduce_2  23# happyReduction_103
happyReduction_103 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	happyIn59
		 (AbsGrammar.MarkPatternOne happy_var_1
	)}

happyReduce_104 = happySpecReduce_3  23# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_3 of { happy_var_3 -> 
	happyIn59
		 (AbsGrammar.MarkPatternMore happy_var_1 happy_var_3
	)}}

happyReduce_105 = happySpecReduce_1  24# happyReduction_105
happyReduction_105 happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (AbsGrammar.MarkPatternHead happy_var_1
	)}

happyReduce_106 = happySpecReduce_3  24# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut58 happy_x_3 of { happy_var_3 -> 
	happyIn60
		 (AbsGrammar.MarkPatternTail happy_var_1 happy_var_3
	)}}

happyReduce_107 = happyReduce 6# 25# happyReduction_107
happyReduction_107 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut63 happy_x_4 of { happy_var_4 -> 
	case happyOut68 happy_x_6 of { happy_var_6 -> 
	happyIn61
		 (AbsGrammar.FunDecl happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_108 = happyReduce 8# 25# happyReduction_108
happyReduction_108 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut63 happy_x_4 of { happy_var_4 -> 
	case happyOut55 happy_x_7 of { happy_var_7 -> 
	case happyOut68 happy_x_8 of { happy_var_8 -> 
	happyIn61
		 (AbsGrammar.FunDeclType happy_var_2 happy_var_4 happy_var_7 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_109 = happySpecReduce_3  26# happyReduction_109
happyReduction_109 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn62
		 (AbsGrammar.Parameter happy_var_1 happy_var_3
	)}}

happyReduce_110 = happySpecReduce_0  27# happyReduction_110
happyReduction_110  =  happyIn63
		 (AbsGrammar.SepPNil
	)

happyReduce_111 = happySpecReduce_1  27# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOut62 happy_x_1 of { happy_var_1 -> 
	happyIn63
		 (AbsGrammar.SepPOne happy_var_1
	)}

happyReduce_112 = happySpecReduce_3  27# happyReduction_112
happyReduction_112 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_1 of { happy_var_1 -> 
	case happyOut64 happy_x_3 of { happy_var_3 -> 
	happyIn63
		 (AbsGrammar.SepPMore happy_var_1 happy_var_3
	)}}

happyReduce_113 = happySpecReduce_1  28# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOut62 happy_x_1 of { happy_var_1 -> 
	happyIn64
		 (AbsGrammar.SepPHead happy_var_1
	)}

happyReduce_114 = happySpecReduce_3  28# happyReduction_114
happyReduction_114 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { happy_var_1 -> 
	case happyOut62 happy_x_3 of { happy_var_3 -> 
	happyIn64
		 (AbsGrammar.SepPTail happy_var_1 happy_var_3
	)}}

happyReduce_115 = happyReduce 5# 29# happyReduction_115
happyReduction_115 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_2 of { happy_var_2 -> 
	case happyOut68 happy_x_3 of { happy_var_3 -> 
	case happyOut68 happy_x_5 of { happy_var_5 -> 
	happyIn65
		 (AbsGrammar.IfElse happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_116 = happySpecReduce_3  30# happyReduction_116
happyReduction_116 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	case happyOut68 happy_x_3 of { happy_var_3 -> 
	happyIn66
		 (AbsGrammar.IfStmt happy_var_2 happy_var_3
	)}}

happyReduce_117 = happySpecReduce_1  31# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOut61 happy_x_1 of { happy_var_1 -> 
	happyIn67
		 (AbsGrammar.FunDeclStmt happy_var_1
	)}

happyReduce_118 = happySpecReduce_2  31# happyReduction_118
happyReduction_118 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn67
		 (AbsGrammar.Stmt happy_var_1
	)}

happyReduce_119 = happySpecReduce_2  31# happyReduction_119
happyReduction_119 happy_x_2
	happy_x_1
	 =  happyIn67
		 (AbsGrammar.Break
	)

happyReduce_120 = happySpecReduce_2  31# happyReduction_120
happyReduction_120 happy_x_2
	happy_x_1
	 =  happyIn67
		 (AbsGrammar.Continue
	)

happyReduce_121 = happySpecReduce_1  31# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOut66 happy_x_1 of { happy_var_1 -> 
	happyIn67
		 (AbsGrammar.If happy_var_1
	)}

happyReduce_122 = happySpecReduce_1  31# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut65 happy_x_1 of { happy_var_1 -> 
	happyIn67
		 (AbsGrammar.IfElseStmt happy_var_1
	)}

happyReduce_123 = happySpecReduce_2  31# happyReduction_123
happyReduction_123 happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_2 of { happy_var_2 -> 
	happyIn67
		 (AbsGrammar.Loop happy_var_2
	)}

happyReduce_124 = happySpecReduce_3  31# happyReduction_124
happyReduction_124 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	case happyOut68 happy_x_3 of { happy_var_3 -> 
	happyIn67
		 (AbsGrammar.While happy_var_2 happy_var_3
	)}}

happyReduce_125 = happyReduce 5# 31# happyReduction_125
happyReduction_125 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	case happyOut68 happy_x_5 of { happy_var_5 -> 
	happyIn67
		 (AbsGrammar.IterableForLoop happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_126 = happyReduce 7# 31# happyReduction_126
happyReduction_126 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	case happyOut40 happy_x_6 of { happy_var_6 -> 
	case happyOut68 happy_x_7 of { happy_var_7 -> 
	happyIn67
		 (AbsGrammar.RangeForLoop happy_var_2 happy_var_4 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_127 = happyReduce 7# 31# happyReduction_127
happyReduction_127 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut58 happy_x_2 of { happy_var_2 -> 
	case happyOut55 happy_x_4 of { happy_var_4 -> 
	case happyOut40 happy_x_6 of { happy_var_6 -> 
	happyIn67
		 (AbsGrammar.LetStmtStrict happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_128 = happyReduce 5# 31# happyReduction_128
happyReduction_128 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut58 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	happyIn67
		 (AbsGrammar.LetStmt happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_129 = happySpecReduce_1  31# happyReduction_129
happyReduction_129 happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	happyIn67
		 (AbsGrammar.BlockStmt happy_var_1
	)}

happyReduce_130 = happySpecReduce_3  32# happyReduction_130
happyReduction_130 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut69 happy_x_2 of { happy_var_2 -> 
	happyIn68
		 (AbsGrammar.Block (reverse happy_var_2)
	)}

happyReduce_131 = happyReduce 4# 32# happyReduction_131
happyReduction_131 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut69 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn68
		 (AbsGrammar.BlockWithValue (reverse happy_var_2) happy_var_3
	) `HappyStk` happyRest}}

happyReduce_132 = happySpecReduce_0  33# happyReduction_132
happyReduction_132  =  happyIn69
		 ([]
	)

happyReduce_133 = happySpecReduce_2  33# happyReduction_133
happyReduction_133 happy_x_2
	happy_x_1
	 =  case happyOut69 happy_x_1 of { happy_var_1 -> 
	case happyOut67 happy_x_2 of { happy_var_2 -> 
	happyIn69
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_134 = happySpecReduce_1  34# happyReduction_134
happyReduction_134 happy_x_1
	 =  case happyOut71 happy_x_1 of { happy_var_1 -> 
	happyIn70
		 (AbsGrammar.Program (reverse happy_var_1)
	)}

happyReduce_135 = happySpecReduce_0  35# happyReduction_135
happyReduction_135  =  happyIn71
		 ([]
	)

happyReduce_136 = happySpecReduce_2  35# happyReduction_136
happyReduction_136 happy_x_2
	happy_x_1
	 =  case happyOut71 happy_x_1 of { happy_var_1 -> 
	case happyOut61 happy_x_2 of { happy_var_2 -> 
	happyIn71
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyNewToken action sts stk [] =
	happyDoAction 43# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TI happy_dollar_dollar) -> cont 40#;
	PT _ (TV happy_dollar_dollar) -> cont 41#;
	PT _ (T_Boolean happy_dollar_dollar) -> cont 42#;
	_ -> happyError' (tk:tks)
	}

happyError_ 43# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pLiteral tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut39 x))

pExpr tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut40 x))

pExpr1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut41 x))

pExpr2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut42 x))

pExpr4 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut43 x))

pExpr6 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut44 x))

pExpr7 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut45 x))

pExpr9 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut46 x))

pExpr10 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut47 x))

pSepExprList tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut48 x))

pSepExprList1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut49 x))

pExpr3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut50 x))

pExpr5 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut51 x))

pExpr8 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut52 x))

pMarkExprList tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut53 x))

pMarkExprList1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut54 x))

pType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut55 x))

pMarkTypeList tks = happySomeParser where
  happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut56 x))

pMarkTypeList1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut57 x))

pPattern tks = happySomeParser where
  happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut58 x))

pMarkPatternList tks = happySomeParser where
  happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (happyOut59 x))

pMarkPatternList1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (happyOut60 x))

pFunDecl tks = happySomeParser where
  happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (happyOut61 x))

pParameter tks = happySomeParser where
  happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (happyOut62 x))

pSepParameterList tks = happySomeParser where
  happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (happyOut63 x))

pSepParameterList1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (happyOut64 x))

pIfElse tks = happySomeParser where
  happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (happyOut65 x))

pIfStmt tks = happySomeParser where
  happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (happyOut66 x))

pStmt tks = happySomeParser where
  happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (happyOut67 x))

pBlock tks = happySomeParser where
  happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (happyOut68 x))

pListStmt tks = happySomeParser where
  happySomeParser = happyThen (happyParse 30# tks) (\x -> happyReturn (happyOut69 x))

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse 31# tks) (\x -> happyReturn (happyOut70 x))

pListFunDecl tks = happySomeParser where
  happySomeParser = happyThen (happyParse 32# tks) (\x -> happyReturn (happyOut71 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "G:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}

















{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\randy\\AppData\\Local\\Temp\\ghc5020_0\\ghc_2.h" #-}
















































































































































{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates\\GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
