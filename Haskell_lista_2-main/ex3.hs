{---
3. (valor 3 pontos) Monada I.O.
---}


---(a) Faca um programa que faca o usuario digitar um numero, e mostre na saıda padrao se ele e par ou ımpar.

main :: IO ()
main = do
  putStrLn "Digite um número:"
  input <- getLine
  let numero = read input :: Integer
  
  if even numero
    then putStrLn "O número é par."
    else putStrLn "O número é ímpar."



---(b) Faca um programa que mostre uma palavra em ordem reversa a partir de uma digitada pelo usuario.

beta :: IO ()
beta = do
  putStrLn "Digite uma palavra:"
  input <- getLine
  let palavraReversa = reverse input
  
  putStrLn ("A palavra ao contrário é: " ++ palavraReversa)


---(c) Faca um programa que calcule uma equacao do segundo grau, a partir dos dados digitados pelo usuario.

import Text.Read (readMaybe)

-- Função para calcular as raízes da equação do segundo grau
calcularRaizes :: Double -> Double -> Double -> (Double, Double)
calcularRaizes a b c =
  let delta = b^2 - 4*a*c
      sqrtDelta = sqrt delta
      x1 = (-b + sqrtDelta) / (2*a)
      x2 = (-b - sqrtDelta) / (2*a)
  in (x1, x2)

main :: IO ()
main = do
  putStrLn "Digite os coeficientes da equação do segundo grau (a b c):"
  input <- getLine
  let maybeCoefs = parseCoefs input
  case maybeCoefs of
    Just (a, b, c) -> do
      let (x1, x2) = calcularRaizes a b c
      putStrLn $ "As raízes da equação são: " ++ show x1 ++ " e " ++ show x2
    Nothing -> putStrLn "Entrada inválida. Certifique-se de fornecer três números separados por espaços."

-- Função para analisar a entrada do usuário e extrair os coeficientes
parseCoefs :: String -> Maybe (Double, Double, Double)
parseCoefs input =
  case words input of
    [a, b, c] -> do
      a' <- readMaybe a
      b' <- readMaybe b
      c' <- readMaybe c
      return (a', b', c')
    _ -> Nothing






---(d) Faca um programa que peca para o usuario entrar com um numero inteiro n e, a partir dele, o usuario deve digitar n linhas e estas devem ser gravadas em um arquivo.

import System.IO

main :: IO ()
main = do
  putStrLn "Digite um número inteiro n:"
  inputN <- getLine
  let maybeN = readMaybe inputN :: Maybe Int
  
  case maybeN of
    Just n -> do
      putStrLn $ "Digite agora as " ++ show n ++ " linhas:"
      linhas <- obterLinhas n []
      escreverArquivo "arquivo.txt" linhas
      putStrLn "As linhas foram gravadas no arquivo."
    Nothing -> putStrLn "Entrada inválida. Certifique-se de fornecer um número inteiro."

-- Função para obter n linhas do usuário
obterLinhas :: Int -> [String] -> IO [String]
obterLinhas n acc
  | n <= 0 = return $ reverse acc
  | otherwise = do
      linha <- getLine
      obterLinhas (n - 1) (linha : acc)

-- Função para escrever as linhas em um arquivo
escreverArquivo :: FilePath -> [String] -> IO ()
escreverArquivo nomeArquivo linhas =
  withFile nomeArquivo WriteMode $ \arquivo -> do
    mapM_ (hPutStrLn arquivo) linhas
