import HCL

main = pim

pim = execReq $ reqConfirm confirm topMenu 
  where
    confirm = promptAgree "Are you sure you want to quit?" (Just False) reqResp

topMenu =
  reqMenu $
  -- Insert a submenu defined elsewhere
  reqSubMenu topMenu "Manage contacts" manageContactsMenu $
  -- Insert a sub menu directly
  reqSubMenu topMenu "Manage calendar"
    (reqMenuItem "Add an event" notImpl $
      reqMenuItem "Remove an event" notImpl $
      reqMenuItem "List events" notImpl $
      reqMenuExit "Return to previous menu"
      reqMenuEnd) $
  -- Insert a submenu and add an extra item to it.
  reqSubMenu topMenu "Manage diary" 
    (manageDiaryMenu $ reqMenuExit "Return to previous menu" $ reqMenuEnd) $
  reqMenuEnd

-- Create a menu through application
manageContactsMenu =
  reqMenuItem "Add a contact" notImpl $
  reqMenuItem "Remove a contact" notImpl $
  reqMenuExit "Return to previous menu"
  reqMenuEnd

-- Create a menu through composition
manageDiaryMenu =
  reqMenuItem "Add an entry" notImpl .
  reqMenuItem "Edit an entry" notImpl 

notImpl = reqIO $ putStrLn "This function is not implemented."