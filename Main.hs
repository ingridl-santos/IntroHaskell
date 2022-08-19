-- Ingrid Lima dos Santos

-- q1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.
soma1 :: Integer -> Integer
soma1 x = x + 1

-- q2. Escreva  uma  função  chamada  sempre  que,  não  importando  o  valor  de  entrada,  devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo.
sempre :: Num a => a -> a
sempre r = 0

-- q3. Escreva  uma  função  chamada  treco  que  receba  três  valores  em  ponto  flutuantes  com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro.
treco :: Double -> Double -> Double -> Double
treco x y z = (x + y) * z

--q4 . Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros.
resto :: Int -> Int -> Int
resto x y = x `mod` y

-- q5. escreva uma funcao chamada precoMaior que devolva o maior valor entre quatro valores monetarios.
precoMaior :: Double -> Double -> Double -> Double -> Double
precoMaior x y z v
  | x > y && x > z && x > v = x
  | y > z && y > v && y > x = y
  | z > v && z > x && z > y = z
  | otherwise = v

-- q6. escreva uma funcao chamada IMPAR que devolva True, sempre que o resultado do produto de dois numeros inteiros for impar.
impar :: Int -> Int -> Bool
impar x y = odd (x * y)

-- q?. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma par :: (Int, Int). Escreva uma funcao em Haskell que devolva a soma dos componentes de um par de inteiros.
somaInt :: Int -> Int -> Int
somaInt x y = x + y

-- q7. escreva uma funcao em Haskell que receba numeros reais (double) e devolva o resultado da equacao x²+y/2 + z.
equacao :: Double -> Double -> Double -> Double
equacao x y z = x ^ 2 + (y / 2) + z

-- q8. escreva uma funcao em Haskell chamada diagnostico que receba o peso do aluno e imprima um diagnostico de obesidade.
diagnostico :: Double -> Double -> String
diagnostico peso altura
  | imc <= 17 = "Muito abaixo do peso"
  | imc > 17 && imc <= 18.49 = "Abaixo do peso"
  | imc > 18.49 && imc <= 24.99 = "Peso normal"
  | imc > 24.99 && imc <= 29.99 = "Sobrepeso"
  | imc > 29.99 && imc <= 34.99 = "Obesidade leve"
  | imc > 34.99 && imc <= 39.99 = "Obesidade severa"
  | otherwise = "Obesidade mórbida"
  where
    imc = peso / altura ^ 2

-- q9. escreva uma funcao em Haskell chamada bissexto que receba um ano e devolva True se o ano for bissexto.
bissexto :: Int -> Bool
bissexto ano
  | mod ano 4 == 0 && (mod ano 100 /= 0 || mod ano 400 == 0) = True
  | otherwise = False

main :: IO ()
main = do
  putStrLn $ "Func. 1: entrada: 2; resultado: " ++ show (soma1 2)
  putStrLn $ "Func. 2: entrada: 2; resultado: " ++ show (sempre 1)
  putStrLn $ "Func. 3: entrada: 1.0 1.0 2.0; resultado: " ++ show (treco 1.0 1.0 2.0)
  putStrLn $ "Func. 4: entrada: 10 3; resultado: " ++ show (resto 10 3)
  putStrLn $ "Func. 5: entrada: 1.5 2.5 3.5 0.5; resultado: " ++ show (precoMaior 1.5 2.5 3.5 0.5)
  putStrLn $ "Func. 6: entrada: 5 3; resultado: " ++ show (impar 5 3)
  putStrLn $ "Func. ?: entrada: 2 2; resultado: " ++ show (somaInt 2 2)
  putStrLn $ "Func. 7: entrada: 1.5 2.5 3.5; resultado: " ++ show (equacao 1.5 2.5 3.5)
  putStrLn $ "Func. 8: entrada: 75 1.61; resultado: " ++ show (diagnostico 75 1.61)
  putStrLn $ "Func. 9: entrada: 2000; resultado: " ++ show (bissexto 2000)
