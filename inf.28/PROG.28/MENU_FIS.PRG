*:*****************************************************************************
*:
*: Procedure file: D:\inf27\PROG27\MENU_fis.PRG
*:         System: Menu-fac
*:         Author: inf27
*:  Last modified: 12/27/2002 at  8:23:42
*:
*:   seleccion de facturas   :    f34r_fis
*:     impresion de facturas :    do rImpreFis in mpath10+'s32fisca' desde f34r_fis
*:
*:
*:
*:          Calls: MPATH+E00PATH.PRG
*:               : MPATH1+W00VARI.PRG
*:               : RENCABEZADO.PRG
*:               : MENU_fis.SPR
*:
*:           Uses: EMPRESA.DBF        
*:               : CONFIENT.DBF       
*:               : CONFISUC.DBF       
*:
*:      Documented 11:20:44                                FoxDoc version 3.00a
*:*****************************************************************************
* ---------------------------------------------------------------------- *
* Amsoft         - Gestión
* Programa       : Menu_int.prg
* Llamado  desde : Escritorio de windows
* Objetivo       : Menu Principal, llama a
* Ultima Actual. : 03/11/2000
* Programo       : Colusso & Garcia
* ---------------------------------------------------------------------- *


CLEAR
*CLEAR ALL
CLOSE ALL
SET AUTOSAVE ON
SET CENTURY ON
SET CLEAR OFF
SET CLOC STATUS
SET CONFIRM ON
SET DELETE ON
SET ESCA OFF
SET EXCL OFF
SET HOUR TO 24
SET KEYCOMP TO DOS
SET NEAR ON
SET TALK OFF
SET readborder OFF
SET SYSMENU OFF
SET SAFETY OFF
SET STATUS BAR OFF
SET STATUS OFF
* on key label RIGHTMOUSE do rMouse_dere

* -------------------------
PUBLIC mclavebo, mclavemo, a_tablas, a_facturas, a_stock,a_ctacte,xtecla, mhabi_jud

STORE 1 TO a_tablas, a_facturas, a_stock,a_ctacte
PUBLIC xx
xx = 0

SELE 0
USE \inf.28\28\empresa
copy to \bs28\28\empresa 
clos data

SELE 0
USE \inf.28\28\empresa



mpath= ALLT(empresa.path)

* mclaveen = empresa.claveen
mclavebo = empresa.clavebo
mclavemo = empresa.clavemo

**mlongi = len(mpath)
*set path to left(mpath,mlongi-1)

mpathx = ALLT(empresa.path0) 

DO mpathx+'e00path'

DO mpath1+'w00vari'
DO rencabezado IN mpath1+'w00wlis'

* on key label ESCAPE do rSalir
msalir = .F.
paso = .T.
mcanticla = 0
xtecla = ' '

* --------- confient -------------
SELE 0
USE \inf.pub\segurida\confient
asuc3 = confient.codsuc

SELE 0
USE \inf.28\28\confisuc

SELE confisuc
SET FILTER TO VAL(confisuc.codsuc) = VAL(asuc3)
SELE confisuc
GO TOP
mclaveen = confisuc.claveen

rele asuc3

* ----  Actualización del correo  -----
* do mpath9+'s32acfon'  with 'ACTU_AUTO'    &&& SE CONFIGURA POR SUCURSAL
* -------------------------------------
DO WHIL .T. AND !msalir

   CLEAR

   SET SYSMENU OFF
  
    if mclaveen = 'S'
       paso = .f.
       do MPATH7+'c11abm'
       if !paso
          * retu 
           
           quit
       ENDI
    endi
   
   IF paso
      DO menu_fis.spr
   ENDI


   EXIT

ENDD
RELE a_tablas
* it
x=0
* quit
RETU
*
*!*****************************************************************************
*!
*!      Procedure: RMOUSE_DERE
*!
*!*****************************************************************************
PROCEDURE rmouse_dere
xtecla = 'MouseDerecho'
KEYB "{ENTER}"
RETU
**
*: EOF: MENU_fis.PRG
