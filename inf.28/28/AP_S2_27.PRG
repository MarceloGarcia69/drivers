

     public mcodseg 
      mcodseg = space(10) 
      mregre = .f.

      do rcla_cod_seg in mpath9+'s70busca'  with ' Actualizar  Precios a Pc Sucursal desde Pendrive !!! ... ','ap_s2_27'  

      if mregre =.t.
         rele mcodseg 
         retu 
      endi

 * --------------   comienzo de actualización    -------------------

   define wind waux from 19,40 to 23,75 color  scheme 1  &&& n+/bg*
set exclusive on
   close data

  acti wind waux
  @1,1 say  ' Actualizando Datos de Precios  ... '

sele 0
use e:\bk_271\actu\articul2 
copy to d:\bs272\272\articulo

Wait Window ' Actualizando  --> ' + dbf()  Timeout 2


sele 0
use e:\bk_271\actu\proveed2 
copy to d:\bs272\272\proveedo
Wait Window ' Actualizando  --> ' + dbf()  Timeout 2

sele 0
use e:\bk_271\actu\marcas2 
copy to d:\bs272\272\marcas
Wait Window ' Actualizando  --> ' + dbf()  Timeout 2

sele 0
use e:\bk_271\actu\rubro2 
copy to d:\bs272\272\rubro
Wait Window ' Actualizando  --> ' + dbf()  Timeout 2

sele 0
use e:\bk_271\actu\subrubr2 
copy to d:\bs272\272\subrubro
Wait Window ' Actualizando  --> ' + dbf()  Timeout 2

sele 0
use e:\bk_271\actu\embalaj2 
copy to d:\bs272\272\embalaje
Wait Window ' Actualizando  --> ' + dbf()  Timeout 2

sele 0
use e:\bk_271\actu\tipuni2 
copy to d:\bs272\272\tipuni
Wait Window ' Actualizando  --> ' + dbf()  Timeout 2

Wait Window ' Se Va a Actualizar  Los Indices de Ventas  --> ' + dbf()  Timeout 2

xx=1
do mpath0+'e70mante'
do mpath9+'s70mante'
xx=0

* -------------------------------

wait ' Proceso de Actualización de Copia - Finalizado  ...  Oprima Una Tecla Para Continuar .... ' wind 
   
   close data	   
   rele wind waux
   set exclusive off
   
     
   
   