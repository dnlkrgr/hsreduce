module HsAllInOne
    ( hsAllInOne
    )
where

import           Data.Traversable               ( for )
import           Language.Haskell.Exts          ( Module(..)
                                                , ModuleHead(..)
                                                , ModuleName(..)
                                                , Name(..)
                                                , QName(..)
                                                , SrcSpanInfo(..)
                                                , ann
                                                , fromParseResult
                                                , importModule
                                                , parseFile
                                                , prettyPrint
                                                )
import           Language.Haskell.Names         ( NameInfo(..)
                                                , Scoped(..)
                                                , annotate
                                                , resolve
                                                , symbolModule
                                                , symbolName
                                                )
import           Data.Generics                  ( everywhere
                                                , extT
                                                , mkT
                                                )
import           Data.List                      ( nub )
import           Data.Char                      ( ord )
import           Data.Hashable                  ( hash )


hsAllInOne :: [FilePath] -> IO ()
hsAllInOne filenames = do
    modules <- for filenames $ fmap fromParseResult . parseFile
    let ours = map getName modules
        env  = resolve modules mempty
        mangled =
            (() <$)
                .   rename ours
                .   removeImports ours
            -- ((()<$) <$>) .
                .   annotate env
                <$> modules
        combined = combine mangled
    putStrLn $ prettyPrint combined


getName (Module _ Nothing _ _ _) = "Main"
getName (Module _ (Just (ModuleHead _ (ModuleName _ n) _ _)) _ _ _) = n

getName' (ModuleName _ n) = n

removeImports :: [String] -> Module l2 -> Module l2
removeImports ours (Module l mh prags imports decls) = Module
    l
    mh
    prags
    (filter go imports)
    decls
    where go i = getName' (importModule i) `notElem` ours

combine mods = Module
    ()
    (Just (ModuleHead () (ModuleName () "Main") Nothing Nothing))
    prags
    imps
    decls
  where
    prags = nub $ concat [ prags | Module _ _ prags _ _ <- mods ]
    imps  = nub $ concat [ imps | Module _ _ _ imps _ <- mods ]
    decls = concat [ decls | Module _ _ _ _ decls <- mods ]

rename :: [String] -> Module (Scoped SrcSpanInfo) -> Module (Scoped SrcSpanInfo)
rename ours x =
    everywhere (mkT (renameName ours (getName x)) `extT` renameQName ours) x

renameQName
    :: [String] -> QName (Scoped SrcSpanInfo) -> QName (Scoped SrcSpanInfo)
renameQName ours (Qual l _ n)
    | Scoped (GlobalSymbol s _) _ <- l
    , let m = getName' (symbolModule s)
    , m `elem` ours
    = UnQual l n
renameQName _ qn = qn


renameName
    :: [String]
    -> String
    -> Name (Scoped SrcSpanInfo)
    -> Name (Scoped SrcSpanInfo)
renameName ours we n
    | Scoped (GlobalSymbol s _) _ <- ann n
    , let m = getName' (symbolModule s)
    , m `elem` ours
    = mangle m (ann n) (symbolName s)
    | Scoped (GlobalSymbol s _) _ <- ann n
    = n
    | Scoped (ScopeError e) _ <- ann n
    = n
    | -- not our name, leave unmodified
      Scoped None _ <- ann n
    = n
    | -- not a normal name, leave unmodified
      otherwise
    = mangle we (ann n) n
--renameName ours we n = error (prettyPrint n ++" : " ++ show (ann n))

modOf :: String -> Scoped () -> Maybe String
modOf we (Scoped (GlobalSymbol s _) _) = Just $ getName' (symbolModule s)
modOf we (Scoped ValueBinder        _) = Just we
modOf we (Scoped (LocalValue _)     _) = Just we
modOf we _                             = Nothing

doubleUS = concatMap go
  where
    go '_' = "__"
    go c   = [c]

doubleCol = concatMap go
  where
    go ':' = "::"
    go c   = [c]

dotToUS = concatMap go
  where
    go '.' = "_"
    go c   = [c]

tosymbol :: Int -> String
tosymbol =
    map ("⚛☃⚾♛♬☏⚒☸☀☮☘☭∞∃" !!) . map (subtract (ord '0')) . map ord . show

mangle :: String -> l2 -> Name l1 -> Name l2
mangle _ l i@(Ident _ "main") = l <$ i -- leave main in place
mangle m l (Ident _ s) = Ident l $ doubleUS s ++ "_" ++ dotToUS (doubleUS m)
mangle m l (Symbol _ s) = Symbol l $ doubleCol s ++ ":" ++ tosymbol (hash m)
