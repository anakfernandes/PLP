import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Definindo tipos de dados
data Recompensa = Ingresso | Comida deriving (Show, Eq, Enum, Bounded, Generic)
instance ToJSON Recompensa
instance FromJSON Recompensa

data Cliente = Cliente
  { email  :: String
  , nome   :: String
  , pontos :: Int
  } deriving (Show, Generic)
instance ToJSON Cliente
instance FromJSON Cliente

-- Constantes para valor das recompensas
valorIngresso :: Int
valorIngresso = 50

valorComida :: Int
valorComida = 30

-- Função para adicionar pontos a um cliente
adicionarPontos :: String -> Int -> [Cliente] -> [Cliente]
adicionarPontos e quantidade [] = [Cliente e "" quantidade]
adicionarPontos e quantidade (c:cs)
  | email c == e = Cliente e (nome c) (pontos c + quantidade) : cs
  | otherwise    = c : adicionarPontos e quantidade cs

-- Função para trocar pontos por recompensas
trocarPontos :: String -> Recompensa -> [Cliente] -> ([Cliente], Maybe String)
trocarPontos e recompensa clientes =
  case lookupCliente e clientes of
    Nothing -> (clientes, Just "Cliente não encontrado")
    Just c  -> 
      let (valor, msg) = casoRecompensa recompensa (pontos c)
      in if pontos c >= valor
         then (adicionarPontos e (-valor) clientes, Nothing)
         else (clientes, Just msg)
  where
    casoRecompensa Ingresso _ = (valorIngresso, "Não há pontos suficientes para trocar por ingresso")
    casoRecompensa Comida _ = (valorComida, "Não há pontos suficientes para trocar por comida")

-- Função para verificar o saldo de pontos do cliente
verificarPontos :: String -> [Cliente] -> Maybe Int
verificarPontos e clientes = pontos <$> lookupCliente e clientes

-- Função auxiliar para procurar um cliente por e-mail
lookupCliente :: String -> [Cliente] -> Maybe Cliente
lookupCliente _ [] = Nothing
lookupCliente e (c:cs)
  | email c == e = Just c
  | otherwise    = lookupCliente e cs

-- Função principal
main :: IO ()
main = do
  let clientes = [Cliente "ana@ccc.ufcg.edu.br" "Ana" 200, Cliente "andreza@ccc.ufcg.edu.br" "Andreza" 100]

  -- Exemplo de operações
  let clientes1 = adicionarPontos "ana@ccc.ufcg.edu.br" 50 clientes
      (clientes2, msg) = trocarPontos "ana@ccc.ufcg.edu.br" Ingresso clientes1
      saldo = verificarPontos "ana@ccc.ufcg.edu.br" clientes2

  print clientes2
  print saldo
  case msg of
    Nothing -> putStrLn "Troca realizada com sucesso!"
    Just m  -> putStrLn m
