module Level4 (
    ) where
data Version = VHDL1993
data T
data NT a
class Rule f a where
  get :: Decorator f => f a
class Monad f => Decorator f where
  n :: [Version] -> f a -> f (NT a)
  chr :: Char -> [Version] -> f T
  txt :: String -> [Version] -> f T
  m :: f a -> f [a]
  c :: [f a] -> f a
  o :: f a -> f (Maybe a)
  trace :: String -> f a -> f a
  n93 :: Rule f a => f (NT a)
  n93 = n [VHDL1993] get
  parenOpen :: f T
  parenOpen = chr '(' [VHDL1993]
  parenClose :: f T
  parenClose = chr ')' [VHDL1993]
  comma :: f T
  comma = chr ',' [VHDL1993]
  moreComma :: Rule f a => f [(T, NT a)]
  moreComma
    = m $ do cc <- comma
             cont <- n93
             return (cc, cont)
type P_MaybeActualParameterPart =
    (Maybe (T, (NT ActualParameterPart), T))
maybeActualParameterPart ::
  Decorator f => f (Maybe (T, NT ActualParameterPart, T))
maybeActualParameterPart
  = o $ do po <- parenOpen
           app <- (n93 :: Decorator f => f (NT ActualParameterPart))
           pc <- parenClose
           return (po, app, pc)
mkNameOrTypeMark ::
  (Decorator m, Rule m a1, Rule m a) =>
  (NT a -> T -> NT a1 -> T -> b) -> m b
mkNameOrTypeMark dcon
  = do name_typemark <- n93
       po <- parenOpen
       fd <- n93
       pc <- parenClose
       return $ dcon name_typemark po fd pc
data ActualDesignator = AD1 (NT Expression) | AD2 (NT Name) | AD3 T
instance Rule f ActualDesignator where
  get
    = trace "ActualDesignator"
        $ {-# SCC "get_ActualDesignator" #-}
          c [AD3 <$> (txt "open" [VHDL1993]), AD2 <$> n93, AD1 <$> n93]
newtype ActualParameterPart = APP (NT AssociationList)
instance Rule f ActualParameterPart where
  get = APP <$> n93
data ActualPart
  = AP1 (NT ActualDesignator) |
    APName (NT Name) T (NT ActualDesignator) T |
    APTypeMark (NT TypeMark) T (NT ActualDesignator) T
instance Rule f ActualPart where
  get
    = trace "ActualPart"
        $ {-# SCC "get_ActualPart" #-}
          c [AP1 <$> n93, mkNameOrTypeMark APName,
             mkNameOrTypeMark APTypeMark]
data Aggregate
  = MkAggregate T (NT ElementAssociation) [(T,
                                            (NT ElementAssociation))] T
instance Rule f Aggregate where
  get
    = do po <- parenOpen
         ea <- n93
         rest <- moreComma
         pc <- parenClose
         return $ MkAggregate po ea rest pc
data Allocator
  = A1 T (NT SubtypeIndication) | A2 T (NT QualifiedExpression)
instance Rule f Allocator where
  get
    = c [A1 <$> (txt "new" [VHDL1993]) <*> n93,
         A2 <$> (txt "new" [VHDL1993]) <*> n93]
data AssociationElement
  = AE (Maybe (NT FormalPart, T)) (NT ActualPart)
instance Rule f AssociationElement where
  get
    = do fp <- o $ do f <- n93
                      a <- txt "=>" [VHDL1993]
                      return (f, a)
         ap <- n93
         return $ AE fp ap
data AssociationList
  = AL (NT AssociationElement) [(T, NT AssociationElement)]
instance Rule f AssociationList where
  get
    = do ae <- n93
         rest <- moreComma
         return $ AL ae rest
data AttributeName
  = AN (NT Prefix) (Maybe (NT TypeMark)) T (Maybe (T,
                                                   (NT Expression), T))
instance Rule f AttributeName where
  get
    = do pp <- n93
         ss <- o n93
         cc <- chr '\'' [VHDL1993]
         ee <- o $ do po <- parenOpen
                      e <- n93
                      pc <- parenClose
                      return (po, e, pc)
         return $ AN pp ss cc ee
data Choice
  = CSmimpleExpression (NT SimpleExpression) |
    CDiscreteRange (NT DiscreteRange) |
    COthers T
instance Rule f Choice where
  get
    = c [CSmimpleExpression <$> n93, CDiscreteRange <$> n93,
         COthers <$> txt "others" [VHDL1993]]
data Constraint
  = CRange (NT RangeConstraint) | CIndex (NT DiscreteRange)
instance Rule f Constraint where
  get = c [CRange <$> n93, CIndex <$> n93]
data DiscreteRange
  = DRSubtypeIndication (NT SubtypeIndication) | DRRange (NT Range)
instance Rule f DiscreteRange where
  get = c [DRSubtypeIndication <$> n93, DRRange <$> n93]
data ElementAssociation = EA (Maybe (NT Choice, T)) (NT Expression)
instance Rule f ElementAssociation where
  get
    = do c <- o $ do c <- n93
                     a <- txt "=>" [VHDL1993]
                     return (c, a)
         e <- n93
         return $ EA c e
data Expression
  = And (NT SimpleExpression) [(T, (NT SimpleExpression))] |
    Or (NT SimpleExpression) [(T, (NT SimpleExpression))] |
    Xor (NT SimpleExpression) [(T, (NT SimpleExpression))] |
    Nand (NT SimpleExpression) (T, (NT SimpleExpression)) |
    Nor (NT SimpleExpression) (T, (NT SimpleExpression)) |
    Xnor (NT SimpleExpression) [(T, (NT SimpleExpression))]
instance Rule f Expression where
  get
    = {-# SCC "get_IndexedName" #-}
      c [And <$> n93 <*> emore "and", Or <$> n93 <*> emore "or",
         Xor <$> n93 <*> emore "xor", Nand <$> n93 <*> etwo "nand",
         Nor <$> n93 <*> etwo "nor", Xnor <$> n93 <*> emore "xnor"]
    where
        etwo tok
          = do n1 <- txt tok [VHDL1993]
               n2 <- n93
               return (n1, n2)
        emore tok
          = do m $ do n2 <- txt tok [VHDL1993]
                      n3 <- n93
                      return (n2, n3)
data Factor
  = FPower (NT Primary) (Maybe (T, (NT Primary))) |
    FAbs T (NT Primary) |
    FNot T (NT Primary)
instance Rule f Factor where
  get
    = trace "Factor"
        $ {-# SCC "get_Factor" #-}
          c [do p <- n93
                rest <- o $ do p <- txt "**" [VHDL1993]
                               p2 <- n93
                               return (p, p2)
                return $ FPower p rest,
             FAbs <$> (txt "abs" [VHDL1993]) <*> n93,
             FNot <$> (txt "not" [VHDL1993]) <*> n93]
newtype FormalDesignator = MkFormalDesignator (NT Name)
instance Rule f FormalDesignator where
  get
    = trace "FormalDesignator"
        $ {-# SCC "get_FormalDesignator" #-} MkFormalDesignator <$> n93
data FormalPart
  = FP1 (NT FormalDesignator) |
    FPName (NT Name) T (NT FormalDesignator) T |
    FPTypeMark (NT TypeMark) T (NT FormalDesignator) T
instance Rule f FormalPart where
  get
    = trace "FormalPart"
        $ {-# SCC "get_FormalPart" #-}
          c [FP1 <$> n93, mkNameOrTypeMark FPName,
             mkNameOrTypeMark FPTypeMark]
data FunctionCall = FC (NT Name) P_MaybeActualParameterPart
instance Rule f FunctionCall where
  get
    = trace "FunctionCall"
        $ {-# SCC "get_FunctionCall" #-}
          do nn <- n93
             app <- maybeActualParameterPart
             return $ FC nn app
data IndexConstraint
  = IC T (NT DiscreteRange) [(T, NT DiscreteRange)] T
instance Rule f IndexConstraint where
  get
    = do po <- parenOpen
         dr <- n93
         rest <- moreComma
         pc <- parenClose
         return $ IC po dr rest pc
data IndexedName
  = IN (NT Prefix) T (NT Expression) [(T, NT Expression)] T
instance Rule f IndexedName where
  get
    = {-# SCC "get_IndexedName" #-}
      do pp <- n93
         po <- parenOpen
         ee <- n93
         ee2 <- moreComma
         pc <- parenClose
         return $ IN pp po ee ee2 pc
data Literal = LNumericLiteral (NT Name) | LNull T
instance Rule f Literal where
  get = c [LNumericLiteral <$> n93, LNull <$> txt "null" [VHDL1993]]
data Name
  = N3 (NT Prefix) | N4 (NT IndexedName) | N6 (NT AttributeName)
instance Rule f Name where
  get
    = trace "Name"
        $ {-# SCC "get_Name" #-} c [N3 <$> n93, N4 <$> n93, N6 <$> n93]
data Prefix
  = PrefixName (NT Name) | PrefixFunctionCall (NT FunctionCall)
instance Rule f Prefix where
  get
    = trace "Prefix"
        $ {-# SCC "get_Prefix" #-}
          c [PrefixName <$> n93, PrefixFunctionCall <$> n93]
data Primary
  = PName (NT Name) |
    PAggregate (NT Aggregate) |
    PFunctionCall (NT FunctionCall) |
    PQualifiedExpression (NT QualifiedExpression) |
    PTypeConversion (NT TypeConversion) |
    PAllocator (NT Allocator) |
    PExpression T (NT Expression) T
instance Rule f Primary where
  get
    = trace "Primary"
        $ {-# SCC "get_Primary" #-}
          c [PName <$> n93, PAggregate <$> n93, PFunctionCall <$> n93,
             PQualifiedExpression <$> n93, PTypeConversion <$> n93,
             PAllocator <$> n93, exp]
    where
        exp
          = do po <- parenOpen
               ee <- n93
               pc <- parenClose
               return $ PExpression po ee pc
data QualifiedExpression
  = QEExpression (NT TypeMark) T T (NT Expression) T |
    EQAggregate (NT TypeMark) T
instance Rule f QualifiedExpression where
  get
    = c [qexp, qagg]
    where
        qexp
          = do tm <- n93
               q <- chr '\'' [VHDL1993]
               po <- parenOpen
               ee <- n93
               pc <- parenClose
               return $ QEExpression tm q po ee pc
        qagg
          = do tm <- n93
               q <- chr '\'' [VHDL1993]
               return $ EQAggregate tm q
data Range
  = R1 (NT AttributeName) |
    R2 (NT SimpleExpression) (NT SimpleExpression)
instance Rule f Range where
  get = c [R1 <$> n93, R2 <$> n93 <*> n93]
data RangeConstraint = RC T (NT Range)
instance Rule f RangeConstraint where
  get
    = do r1 <- txt "range" [VHDL1993]
         r2 <- n93
         return $ RC r1 r2
data Relation
  = R (NT SimpleExpression) (Maybe ((NT SimpleExpression)))
instance Rule f Relation where
  get
    = do se <- n93
         rest <- o $ do se <- n93
                        return se
         return $ R se rest
data ShiftExpression
  = ShiftE (NT SimpleExpression) (Maybe ((NT SimpleExpression)))
instance Rule f ShiftExpression where
  get
    = do se <- n93
         rest <- o $ do se <- n93
                        return se
         return $ ShiftE se rest
data SimpleExpression = SimpleE (NT Primary) [(NT Primary)]
instance Rule f SimpleExpression where
  get
    = do tt <- n93
         rest <- m $ do tt2 <- n93
                        return tt2
         return $ SimpleE tt rest
data SliceName = SliceNPrefix (NT DiscreteRange)
instance Rule f SliceName where
  get = SliceNPrefix <$> n93
data SubtypeIndication
  = SI (Maybe (NT Name)) (NT TypeMark) (Maybe (NT Constraint))
instance Rule f SubtypeIndication where
  get
    = trace "SubtypeIndication"
        $ {-# SCC "get_SubtypeIndication" #-}
          do nn <- o n93
             tm <- n93
             cc <- o n93
             return $ SI nn tm cc
data TypeConversion
  = MkTypeConversion (NT TypeMark) T (NT Expression) T
instance Rule f TypeConversion where
  get
    = do tm <- n93
         po <- parenOpen
         e <- n93
         pc <- parenClose
         return $ MkTypeConversion tm po e pc
data TypeMark = TM1 (NT Name) | TM2 (NT Name)
instance Rule f TypeMark where
  get
    = trace "TypeMark"
        $ {-# SCC "get_TypeMark" #-} c [TM1 <$> n93, TM2 <$> n93]