import System.Exit

-- User DataBase --
data Users = Users { name :: String
                    ,uname :: String
                    ,pass :: String
                    ,address :: String
                    ,contact :: Int
                  } deriving (Show, Eq, Read)

user1 = Users{name = "Mithlesh", uname = "Mike", pass = "Mikeee", address = "Dighi", contact = 7276736058}
user2 = Users{name = "Binodi ", uname = "Bin", pass = "Bin123", address = "Pune", contact = 9987654601}
user3 = Users{name = "Ramlal ", uname = "Ram", pass = "Ram123", address = "PCMC", contact = 8394284563}
user4 = Users{name = "Shayam ", uname = "Shy", pass = "Shy123", address = "Dighi", contact = 9029545760}
user5 = Users{name = "Sarthi ", uname = "Sar", pass = "Sar123", address = "Nigdi", contact = 9771245098}

user_database = [user1, user2, user3, user4, user5]

showData::[Users]->String
showData [] = ""
showData (x:xs) | xs == [] ="+---------------------------------------------------------------+\n|" ++ (name $ x) ++ "\t| " ++ (uname $ x) ++ "\t| " 
                           ++ (pass $ x) ++ "\t| " ++ (address $ x) ++ "\t| "  
                           ++ (show (contact $ x))++ "\t|\n+---------------------------------------------------------------+\n"
                | otherwise = display
                where
                 display = "+---------------------------------------------------------------+\n|"++(name $ x) ++ "\t| " ++ (uname $ x) ++ "\t| " 
                           ++ (pass $ x) ++ "\t| " ++ (address $ x) ++ "\t| "  
                           ++ (show (contact $ x))++ "\t|\n" ++ (showData xs)

searchUser::[Users]->String->String
searchUser [] username = "No result"
searchUser (x:xs) username | xs == [] = if (check==True) then fetchData else "No result"
                           | otherwise = if (check==True) then fetchData else (searchUser xs username)
                                        where
                                         check     = if ((uname $ x)==username) then True else False
                                         fetchData = "+---------------------------------------------------------------+\n|"
                                                      ++ (name $ x) ++ "\t| " ++ (uname $ x) ++ "\t| " 
                                                      ++ (pass $ x) ++ "\t| " ++ (address $ x) ++ "\t| "  
                                                      ++ (show (contact $ x))
                                                      ++ "\t|\n+---------------------------------------------------------------+\n"

login :: [Users] -> IO ()
login userList = do{
          putStrLn "=========================";
          putStrLn "|\tLogin Page\t|\n=========================\nUsername:\t\t|";
          uname <- getLine::IO String;
          putStrLn "Password:\t\t|";
          pass <- getLine::IO String;
          putStrLn "=========================";
          if ((validate userList uname pass)==True) then
              do{
                print ("Welcome " ++  uname);
                putStrLn "+---------------------------------------------------------------+";
                putStrLn "|\t1. View all Users\t|\t2. Search User\t\t|";
                putStrLn "+-------------------------------+-------------------------------+";
                putStrLn "|\t3. Sign Out \t\t|\t4. Quit\t\t\t|";
                putStrLn "+---------------------------------------------------------------+";
              
                c <- getLine::IO String;
                if (c == "1") then
                  do{
                     putStrLn "+---------------------------------------------------------------+\n|Name   \t|Uname \t|Password\t|Address| Contact\t|";
                     putStrLn (showData userList);
                     exitWith (ExitFailure 43);
                   }
                else if (c == "2") then
                     do
                      putStrLn "\n+-------------------------------+\n|\tSearch by Username\t|";
                      putStrLn "+-------------------------------+"
                      name <- getLine::IO String;
                      putStrLn (searchUser userList name);
                      exitWith (ExitFailure 43);
                else if (c == "3") then
                  main;
                else if (c == "4") then
                  exitWith (ExitFailure 43);
                else
                 return ();
              }

          else
              putStrLn "\nInvalid username or Password !\n==============================\nWould you like to try again[y/n]:";
              ch <- getLine::IO String;
              if (ch=="y" || ch == "Y") then
                 login userList;
              else
                return ();
}

signup::IO ()
signup = do
  putStrLn "=================================";
  putStrLn "|\tRegister page\t\t|\n| Please Provide below details  |\n=================================\nEnter Your name:\t\t|";
  name <- getLine::IO String;
  putStrLn "_________________________________";
  putStrLn "Enter Username:\t\t\t|";
  uname <- getLine::IO String;
  putStrLn "_________________________________";
  putStrLn "Enter Password:\t\t\t|";
  pass <- getLine::IO String;
  putStrLn "_________________________________";
  putStrLn "Enter Address:\t\t\t|";
  address <- getLine::IO String;
  putStrLn "_________________________________";
  putStrLn "Enter Contact:\t\t\t|";
  contact <- getLine::IO String;
  putStrLn "=================================";
  putStrLn "|\tRegister Succesfully\t|";
  putStrLn "=================================\n\n";
  let updated_user_database = user_database ++ [Users {name = name, uname = uname, pass = pass, address = address, contact = (read contact::Int)}]
  -- putStrLn (show updated_user_database)
  login updated_user_database
  return ();

validate::[Users] -> String->String->Bool
validate userList unm pas = if True `elem` [if (((uname x) == unm) && ((pass x)==pas))then True else False | x <- userList]
                      then True
                    else
                      False

main::IO ()
main = do{
 putStrLn "+----------------------------------------+\n| Please Use below Link to start Haskell |\n|   http://learnyouahaskell.com/chapters |\n+----------------------------------------+";
 putStrLn "=========================================";
 putStrLn "|\tWelcome To Haskell webiste\t|\n|\t\t1. Login\t\t|\n|\t\t2. Sign up\t\t|\n=========================================\n|\tWhat would you like to do\t|";
 putStrLn "=========================================";
 n <- readLn;
 if n==1 then
    login user_database
 else if n==2 then
    signup
  else
    putStrLn "Invalid choice";
}