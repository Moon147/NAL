# NAL
Para descargar el proyecto:
1. Crear carpeta local
2. Posicionarse en la carpeta creada.
3. Descargar proyecto desde la terminal con:
    $ git clone https://github.com/Moon147/NAL-Reasoner.git

Para revisar cambios con la rama que se está trabajando:
1. Posicionarse en la carpeta del proyecto.
2. Escribir en la terminal:
    $ git status

Para actualizar el proyecto local con los cambios en la rama que se está trabajando:
1. Posicionarse en la carpeta del proyecto.
2. Verificar estado de los archivos locales:
    $ git status
3. Obtener cambios en la rama:
    $ git pull origin nombre_rama

Para enviar cambios a la rama donde se está trabajando:
1. Posicionarse en la carpeta del proyecto.
2. Verificar qué archivos fueron modificados:
    $ git status
3. Agregar archivos para actualizar la rama:
    $ git add nombre_archivo
4. Hacer commit:
    $ git commit -m 'modificaciones que se le hicieron a los archivos'
5. Enviar cambios a la rama:
    $ git push origin nombre_rama
6. Verificar que los cambios se realizaron:
    $ git log
7. Verificar que los archivos locales estén actualizados:
    $ git status
