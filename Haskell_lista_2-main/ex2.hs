{---
2. (valor 3 pontos) Sobre monadas.

2.1. Faca um tipo Caixa com um type parameter a e tres construtores chamados Um , Dois e Tres possuindo
um, dois e tres campos de tipo a , respectivamente.

• Faca uma instancia de Functor para o tipo Caixa. A funcao deve ser aplicada em todas as coordenadas
dos valores ( Um , Dois ou Tres ).


• Crie uma instancia de Monad para o tipo Caixa. Seu return deve ser o value constructor de Um .
Observacao: quando definir >>= para Caixa , o valor a para entrar em f, "(>>=) :: m a -> (a -> m b) -> m b”,
segue as regras:
• Um: o unico campo entra na funcao (analogo ao Maybe);
• Dois: o segundo campo entra;
• Tres: o terceiro campo entra.
---}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

data Caixa a = Um a | Dois a a | Tres a a a deriving Show

-- Instância de Functor para o tipo Caixa
instance Functor Caixa where
    fmap f (Um a) = Um (f a)
    fmap f (Dois a b) = Dois (f a) (f b)
    fmap f (Tres a b c) = Tres (f a) (f b) (f c)

-- Instância de Monad para o tipo Caixa
instance Monad Caixa where
    return = Um
    (Um a) >>= f = f a
    (Dois a b) >>= f = case f b of
                Um x -> Dois a x
                _ -> error "Função não compatível com o construtor Dois."

    (Tres a b c) >>= f = case f c of
                           Um x -> Tres a b x
                           _ -> error "Função não compatível com o construtor Tres."


{--
2.2. Crie uma funcao mult234 :: Double -> Caixa Double que receba uma parametro x e devolva o dobro de
x na primeira coordenada, o triplo na segunda e o quadruplo na terceira usando o operador >>=
--}



mult234 :: Double -> Caixa Double
mult234 x = Um x >>= \a ->
             Dois (a * 2) a >>= \b ->
             Tres (b * 3) (b * 3) (b * 3) >>= \c ->
             return (c * 4)
