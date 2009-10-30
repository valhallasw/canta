; FRANCAIS + COMPLET

; Suppression de l'exe dans le répertoire 'Install'
!system 'del D:\Work\Dev\Canta\Install\canta-fr.exe'
; Compression de l'exe de Exe dans Install
!system 'D:\Developpement\NeoLite\NeoLiteCli D:\Work\Dev\Canta\Exe\canta-fr.exe D:\Work\Dev\Canta\Install\canta-fr.exe -e -q'

; HM NIS Edit Wizard helper defines
!define PRODUCT_NAME "Canta"
!define PRODUCT_VERSION "1.10"
!define PRODUCT_PUBLISHER "Chaumet Software"
!define PRODUCT_WEB_SITE "http://www.chaumetsoftware.com"
!define PRODUCT_DIR_REGKEY "Software\Microsoft\Windows\CurrentVersion\App Paths\canta.exe"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"

SetCompressor lzma

; MUI 1.67 compatible ------
!include "MUI.nsh"

; bitmap affichée sur la côté gauche au démarrage et quand l'instal est terminée
!define MUI_WELCOMEFINISHPAGE_BITMAP "inst_welcome.bmp"

; MUI Settings
!define MUI_ABORTWARNING
!define MUI_ICON "canta.ico"
!define MUI_UNICON "canta_uninst.ico"

; Welcome page
!insertmacro MUI_PAGE_WELCOME
; License page
!define MUI_LICENSEPAGE_CHECKBOX
!insertmacro MUI_PAGE_LICENSE "licence-fr.txt"
; Directory page
!insertmacro MUI_PAGE_DIRECTORY
; Instfiles page
!insertmacro MUI_PAGE_INSTFILES
; Finish page
!define MUI_FINISHPAGE_RUN "$INSTDIR\canta.exe"
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages
!insertmacro MUI_UNPAGE_INSTFILES

; Language files
!insertmacro MUI_LANGUAGE "French"

; Reserve files
!insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

; MUI end ------

Name "${PRODUCT_NAME} ${PRODUCT_VERSION}"
OutFile "..\..\exe\InstallCanta-fr-v${PRODUCT_VERSION}.exe"
InstallDir "$PROGRAMFILES\Canta"
InstallDirRegKey HKLM "${PRODUCT_DIR_REGKEY}" ""
ShowInstDetails show
ShowUnInstDetails show


Section "SectionPrincipale" SEC01
  SetOutPath "$INSTDIR"
  SetOverwrite try
  File /oname=canta.exe "D:\Work\Dev\Canta\Install\canta-fr.exe"
  CreateDirectory "$SMPROGRAMS\Canta"
  CreateShortCut "$SMPROGRAMS\Canta\Canta.lnk" "$INSTDIR\canta.exe"
  CreateShortCut "$DESKTOP\Canta.lnk" "$INSTDIR\canta.exe"
  
  ; Fichiers de l'aide
  SetOutPath "$INSTDIR\help"
  File "..\..\Install\help\*.*"

  ; Descriptions de skins
  SetOutPath "$INSTDIR\skins"
  File /r "..\..\Install\skins\*.*"

  ; Fichier de licence
  SetOutPath "$INSTDIR"
  File "licence-fr.txt"
SectionEnd

Section -AdditionalIcons
  WriteIniStr "$INSTDIR\${PRODUCT_NAME}.url" "InternetShortcut" "URL" "${PRODUCT_WEB_SITE}"
  CreateShortCut "$SMPROGRAMS\Canta\Website.lnk" "$INSTDIR\${PRODUCT_NAME}.url"
  CreateShortCut "$SMPROGRAMS\Canta\Uninstall.lnk" "$INSTDIR\uninst.exe"
SectionEnd

Section -Post
  WriteUninstaller "$INSTDIR\uninst.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REGKEY}" "" "$INSTDIR\canta.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "$(^Name)"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\uninst.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayIcon" "$INSTDIR\canta.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"

  ; Base de registres
  WriteRegStr HKLM "software\Chaumet\Canta" "Exedir" "$INSTDIR"
  WriteRegStr HKLM "software\Chaumet\Canta" "Skin" "default"

SectionEnd


Function un.onUninstSuccess
  HideWindow
  MessageBox MB_ICONINFORMATION|MB_OK "$(^Name) a été désinstallé avec succès de votre ordinateur."
FunctionEnd

Function un.onInit
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "Êtes-vous certains de vouloir désinstaller totalement $(^Name) et tous ses composants ?" IDYES +2
  Abort
FunctionEnd

Section Uninstall
  Delete "$INSTDIR\${PRODUCT_NAME}.url"
  Delete "$INSTDIR\uninst.exe"
  Delete "$INSTDIR\licence-fr.txt"
  Delete "$INSTDIR\log.txt"
  Delete "$INSTDIR\debug.txt"
  ; Suppression de l'executable
  Delete "$INSTDIR\canta.exe"

  ; Desinstallation de l'aide
  Delete "$INSTDIR\help\*.*"
  RMDir   "$INSTDIR\help"
  
  ; Desinstallation des skins
  Delete "$INSTDIR\skins\massive\*.*"
  Delete "$INSTDIR\skins\lounge\*.*"
  Delete "$INSTDIR\skins\kontakt\*.*"
  Delete "$INSTDIR\skins\fruity\*.*"
  Delete "$INSTDIR\skins\*.*"

  ; Suppression des répertoire des skins
  RMDir "$INSTDIR\skins\massive"
  RMDir "$INSTDIR\skins\lounge"
  RMDir "$INSTDIR\skins\kontakt"
  RMDir "$INSTDIR\skins\fruity"
  RMDir "$INSTDIR\skins\"

  ; Supression des répertoire
  RMDir "$INSTDIR"

  ; Suppression des liens
  Delete "$SMPROGRAMS\Canta\Uninstall.lnk"
  Delete "$SMPROGRAMS\Canta\Website.lnk"
  Delete "$DESKTOP\Canta.lnk"
  Delete "$SMPROGRAMS\Canta\Canta.lnk"
  RMDir "$SMPROGRAMS\Canta"

  ; Suppression des entrées de la base de registre
  DeleteRegKey HKLM "SOFTWARE\Chaumet\Canta"

  ; suppression dans les registre de desinstallation
  DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"
  DeleteRegKey HKLM "${PRODUCT_DIR_REGKEY}"

  SetAutoClose true
SectionEnd