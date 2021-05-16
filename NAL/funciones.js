//--------------------------------------------CÓDIGO PUSHBAR------------------------------------------------------
class Pushbar {
  constructor(config = { overlay: true, blur: false }) {
    this.activeBar = null;
    this.overlay = false;

    if (config.overlay) {
      this.overlay = document.createElement('div');
      this.overlay.classList.add('pushbar_overlay');
      document.querySelector('body').appendChild(this.overlay);
    }

    if (config.blur) {
      const mainContent = document.querySelector('.pushbar_main_content');
      if (mainContent) {
        mainContent.classList.add('pushbar_blur');
      }
    }

    this.bindEvents();
  }

  get opened() {
    const { activeBar } = this;
    return Boolean(activeBar instanceof HTMLElement && activeBar.classList.contains('opened'));
  }
  
  get activeBarId() {
    const { activeBar } = this;
    return activeBar instanceof HTMLElement && activeBar.getAttribute('data-pushbar-id');
  }

  static dispatchOpen(pushbar) {
    const event = new CustomEvent('pushbar_opening', { bubbles: true, detail: { pushbar } });
    pushbar.dispatchEvent(event);
  }

  static dispatchClose(pushbar) {
    const event = new CustomEvent('pushbar_closing', { bubbles: true, detail: { pushbar } });
    pushbar.dispatchEvent(event);
  }

  static findElementById(pushbarId) {
    return document.querySelector(`[data-pushbar-id="${pushbarId}"]`);
  }

  handleOpenEvent(e) {
    e.preventDefault();
    const pushbarId = e.currentTarget.getAttribute('data-pushbar-target');
    if (pushbarId) {
      this.open(pushbarId);
    }
  }

  handleCloseEvent(e) {
    e.preventDefault();
    this.close();
  }

  handleKeyEvent(e) {
    if (this.opened && e.keyCode === 27) {
      this.close();
    }
  }

  bindEvents() {
    const triggers = document.querySelectorAll('[data-pushbar-target]');
    const closers = document.querySelectorAll('[data-pushbar-close]');

    triggers.forEach(trigger => trigger.addEventListener('click', e => this.handleOpenEvent(e), false));
    closers.forEach(closer => closer.addEventListener('click', e => this.handleCloseEvent(e), false));

    if (this.overlay) {
      this.overlay.removeEventListener('click', function(){console.log("");} , true);
    }
    document.addEventListener('keyup', e => this.handleKeyEvent(e));
  }

  open(pushbarId) {
    // Current bar is already opened
    if (String(pushbarId) === this.activeBarId && this.opened) {
      return;
    }
    
    // Get new pushbar target
    const pushbar = Pushbar.findElementById(pushbarId);

    if (!pushbar) return;
    
    // Close active bar (if exists)
    if (this.opened) {
      this.close();
    }
    
    Pushbar.dispatchOpen(pushbar);
    pushbar.classList.add('opened');

    const Root = document.querySelector('html');
    Root.classList.add('pushbar_locked');
    Root.setAttribute('pushbar', pushbarId);
    this.activeBar = pushbar;
  }

  close() {
    const { activeBar } = this;
    if (!activeBar) return;
    
    Pushbar.dispatchClose(activeBar);
    activeBar.classList.remove('opened');

    const Root = document.querySelector('html');
    Root.classList.remove('pushbar_locked');
    Root.removeAttribute('pushbar');
    
    this.activeBar = null;
  }
}


new Pushbar({
        blur:true,
        overlay:true,
      });

//-------------------------------------------LLENADO DE CONTENIDO---------------------------------------------

var conocimiento=document.querySelector("#conocimiento");
var informacion=document.querySelector("#informacion");
var id=1;


function almacena(){
  //alert("que pasaaaaa");
  //console.log("activo2");
  var xhr = new XMLHttpRequest();
  xhr.open("GET", "BC/BC0.txt", true);
  xhr.send();
  xhr.onreadystatechange = function(){
    if (this.readyState == 4 && this.status == 200){
      console.log(this.responseText);
      //conocimiento.value = this.responseText;
    }
  };



  /*if(conocimiento.value!=""){
    informacion.innerHTML=informacion.innerHTML+conocimiento.value+"<br>";
    var tabla=document.querySelector("#TABLA");
    var tbody= tabla.tBodies[0];
    var fila=document.createElement("tr");
    var celda1=document.createElement("td");
    var celda2=document.createElement("td");
    var celda3=document.createElement("td");
    celda1.innerHTML=id;
    celda2.innerHTML=conocimiento.value;
    celda3.innerHTML= "(0.0 0.0)";
    fila.appendChild(celda1); fila.appendChild(celda2); fila.appendChild(celda3);
    tbody.appendChild(fila);
    conocimiento.value="";
  }
  id++;*/
}

//---------------------------------------------TABS BC Y MEDIO--------------------------------------------------
function openTab(evt, tabName) {
  var i, tabcontent, tabs__item;
  tabcontent = document.getElementsByClassName("tabcontent");
  
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = "none";
  }
  tabs__item = document.getElementsByClassName("tabs__item");
  for (i = 0; i < tabs__item.length; i++) {
    tabs__item[i].className = tabs__item[i].className.replace(" active", "");
    
  }
  document.getElementById(tabName).style.display = "flex";
  evt.currentTarget.className += " active";
}

function openTab2(evt, tabName) {
  var i, tabcontent, tabs__item;
  tabcontent = document.getElementsByClassName("tabcontent2");
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = "none";
  }
  tabs__item = document.getElementsByClassName("tabs__item2");
  for (i = 0; i < tabs__item.length; i++) {
    tabs__item[i].className = tabs__item[i].className.replace(" active", "");
  }
  document.getElementById(tabName).style.display = "block";
  evt.currentTarget.className += " active";
}

//----------------------------------------------BOTONES----------------------------------------------------------

function simbolo(simb){
  conocimiento.value += " "+ simb+ " ";

}

//----------------------------------------------MENU--------------------------------------------------------------

function display_pushbar(tabName){
  tabcontent= document.querySelectorAll(".sub-menu");
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = "none";
  }
  document.querySelector(".menu").style.display= "none";
  document.querySelector(".cerrar").style.display= "none";
  document.querySelector(".regresar").style.display= "block";
  document.querySelector(tabName).style.display= "block";
}

function back(){
  tabcontent= document.querySelectorAll(".sub-menu");
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = "none";

  document.querySelector(".menu").style.display= "block";
  document.querySelector(".cerrar").style.display= "block";
  document.querySelector(".regresar").style.display= "none";
  }
}


// ----------------------SCROLL--------
function scroll()
{
    //Obtengo el div
    var e = document.getElementById('TABLA');
    //Llevo el scroll al fondo
    var objDiv = document.getElementById("TABLA");
    objDiv.scrollTop = objDiv.scrollHeight;
}

//---------------------------------------------TABLA COLLAPSE--------------------------------------------------
function collapse() {
  const aside = document.getElementById("collapse");
  const close = document.getElementById("closeCollapse");
  const open = document.getElementById("openCollapse");
  const Root = document.querySelector('html');
  Root.classList.add('pushbar_locked');
  aside.classList.remove("aside-close");
  aside.classList.add("aside-open");
  open.classList.add("closeCollapse");
  open.classList.remove("openCollapse");
  close.classList.add("openCollapse");
  close.classList.remove("closeCollapse");
}

function collapseOff() {
  const aside = document.getElementById("collapse");
  const close = document.getElementById("closeCollapse");
  const open = document.getElementById("openCollapse");
  const Root = document.querySelector('html');
  Root.classList.remove('pushbar_locked');
  aside.classList.add("aside-close");
  aside.classList.remove("aside-open");
  close.classList.add("closeCollapse");
  close.classList.remove("openCollapse");
  open.classList.add("openCollapse");
  open.classList.remove("closeCollapse");
}

function recuperarPolitica(){
var politicaControl = new Object();
console.log("Obteniendo política");
objSerialized = localStorage.getItem('politica'); 

politicaControl = JSON.parse(objSerialized);
if(politicaControl.agregar =="on"){
	$("#lpopcadd").prop("checked", true);
}else{
	$("#lpopcadd").prop("checked", false);
}
document.getElementById("lpdecimales").value=politicaControl.decimales;
document.getElementById("lpkuser").value=politicaControl.k;
document.getElementById("lpcomportamiento").value=politicaControl.comportamiento;
document.getElementById("lprepetidos").value=politicaControl.inferenciaRepetidos;

document.getElementById("inferenciaRepetidos").value=politicaControl.inferenciaRepetidos;
document.getElementById("comportamiento").value=politicaControl.comportamiento;
document.getElementById("decimales").value=politicaControl.decimales;
document.getElementById("kuser").value=politicaControl.k;
document.getElementById("opcadd").value=politicaControl.agregar;
}

$("#lpopcadd").change(function() {
	if(document.politica.lpopcadd.checked){
		document.getElementById("lpopcadd").value = "on";
	}else {
		document.getElementById("lpopcadd").value="off";}
	console.log(document.getElementById("lpopcadd").value);		
});

jQuery(document).ready(function() {
  var politicaControl = new Object();
  jQuery('#save').on('click',function() {    
    politicaControl.agregar = document.getElementById("lpopcadd").value;
    politicaControl.decimales = document.getElementById("lpdecimales").value;
    politicaControl.k = document.getElementById("lpkuser").value;
    politicaControl.comportamiento = document.getElementById("lpcomportamiento").value;
    politicaControl.inferenciaRepetidos = document.getElementById("lprepetidos").value;
    objSerialized = JSON.stringify(politicaControl);
    localStorage.setItem('politica', objSerialized);
    location.reload();

  });

});
