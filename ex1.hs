{--
1. (valor 3 pontos) Sobre funtores.
Dado o tipo de dado algebrico:
data Coisa a = UmaCoisa a | DuasCoisas a a | ZeroCoisa deriving Show
Faca as seguintes implementacoes:
---}

{---
(a) Faca uma instancia de Functor para o tipo Coisa. A funcao g deve ”ir para dentro” em todas as coordenadas de Coisa . No caso de ZeroCoisa, o fmap deve retornar ZeroCoisa.
---}

data Coisa a = UmaCoisa a | DuasCoisas a a | ZeroCoisa deriving Show

instance Functor Coisa where
    fmap g (UmaCoisa a) = UmaCoisa (g a)
    fmap g (DuasCoisas a b) = DuasCoisas (g a) (g b)
    fmap _ ZeroCoisa = ZeroCoisa

{---
(b) Aproveitando o exercıcio anterior, faca uma instancia de Applicative Functor para o tipo Coisa.
---}

instance Applicative Coisa where
    pure a = UmaCoisa a
    (UmaCoisa g) <*> coisa = fmap g coisa
    (DuasCoisas g1 g2) <*> coisa = DuasCoisas (g1 <$> coisa) (g2 <$> coisa)
    <*> ZeroCoisa = ZeroCoisa


{--
(c) Crie a funcao " mult234 :: Double -> Coisa Double " que multiplica por 2 a primeira coordenada, por 3 a segunda, e por 4 a terceira. Use a instancia de Applicative feita no exercıcio anterior. 
---}

mult234 :: Double -> Double -> Double -> Coisa Double
mult234 x y z = DuasCoisas (*2*x) (*3*y) <*> UmaCoisa z

{--
(d) Escreva uma instancia para Functor e Applicative para o tipo Arvore da lista de exercıcio anterior.
--}

data Arvore a = No a [Arvore a] deriving Show

instance Functor Arvore where
    fmap g (No x subArvores) = No (g x) (fmap (fmap g) subArvores)

instance Applicative Arvore where
    pure x = No x []
    (No g subFs) <*> arvore = No (g (fst <$> arvore)) (zipWith (<*>) subFs (snd <$> arvore))
