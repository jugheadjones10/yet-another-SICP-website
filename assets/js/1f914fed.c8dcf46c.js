"use strict";(self.webpackChunksicp_docu=self.webpackChunksicp_docu||[]).push([[939],{3905:function(e,r,n){n.d(r,{Zo:function(){return p},kt:function(){return f}});var t=n(7294);function a(e,r,n){return r in e?Object.defineProperty(e,r,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[r]=n,e}function o(e,r){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);r&&(t=t.filter((function(r){return Object.getOwnPropertyDescriptor(e,r).enumerable}))),n.push.apply(n,t)}return n}function i(e){for(var r=1;r<arguments.length;r++){var n=null!=arguments[r]?arguments[r]:{};r%2?o(Object(n),!0).forEach((function(r){a(e,r,n[r])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(r){Object.defineProperty(e,r,Object.getOwnPropertyDescriptor(n,r))}))}return e}function c(e,r){if(null==e)return{};var n,t,a=function(e,r){if(null==e)return{};var n,t,a={},o=Object.keys(e);for(t=0;t<o.length;t++)n=o[t],r.indexOf(n)>=0||(a[n]=e[n]);return a}(e,r);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(t=0;t<o.length;t++)n=o[t],r.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var s=t.createContext({}),l=function(e){var r=t.useContext(s),n=r;return e&&(n="function"==typeof e?e(r):i(i({},r),e)),n},p=function(e){var r=l(e.components);return t.createElement(s.Provider,{value:r},e.children)},u={inlineCode:"code",wrapper:function(e){var r=e.children;return t.createElement(t.Fragment,{},r)}},d=t.forwardRef((function(e,r){var n=e.components,a=e.mdxType,o=e.originalType,s=e.parentName,p=c(e,["components","mdxType","originalType","parentName"]),d=l(n),f=a,m=d["".concat(s,".").concat(f)]||d[f]||u[f]||o;return n?t.createElement(m,i(i({ref:r},p),{},{components:n})):t.createElement(m,i({ref:r},p))}));function f(e,r){var n=arguments,a=r&&r.mdxType;if("string"==typeof e||a){var o=n.length,i=new Array(o);i[0]=d;var c={};for(var s in r)hasOwnProperty.call(r,s)&&(c[s]=r[s]);c.originalType=e,c.mdxType="string"==typeof e?e:a,i[1]=c;for(var l=2;l<o;l++)i[l]=n[l];return t.createElement.apply(null,i)}return t.createElement.apply(null,n)}d.displayName="MDXCreateElement"},1777:function(e,r,n){n.r(r),n.d(r,{frontMatter:function(){return c},contentTitle:function(){return s},metadata:function(){return l},toc:function(){return p},default:function(){return d}});var t=n(7462),a=n(3366),o=(n(7294),n(3905)),i=["components"],c={sidebar_position:1},s="Multiple Representations for Abstract Data",l={unversionedId:"chapter-2/section-2.4",id:"chapter-2/section-2.4",isDocsHomePage:!1,title:"Multiple Representations for Abstract Data",description:'This section uses the procedures "put" and "get" a lot, for manipulating operation-and-type tables. The code for these 2 procedures aren\'t introduced until later in the book, but we need them for testing our exercise code, so here they are.',source:"@site/docs/chapter-2/section-2.4.md",sourceDirName:"chapter-2",slug:"/chapter-2/section-2.4",permalink:"/chapter-2/section-2.4",editUrl:"https://github.com/facebook/docusaurus/edit/master/website/docs/chapter-2/section-2.4.md",version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",previous:{title:"Introduction",permalink:"/"},next:{title:'"Breadth" and drowning in information',permalink:"/chapter-2/breadth"}},p=[{value:"Exercise 2.73",id:"exercise-273",children:[]}],u={toc:p};function d(e){var r=e.components,n=(0,a.Z)(e,i);return(0,o.kt)("wrapper",(0,t.Z)({},u,n,{components:r,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"multiple-representations-for-abstract-data"},"Multiple Representations for Abstract Data"),(0,o.kt)("p",null,'This section uses the procedures "put" and "get" a lot, for manipulating operation-and-type tables. The code for these 2 procedures aren\'t introduced until later in the book, but we need them for testing our exercise code, so here they are.'),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scheme"},"(define (assoc key records)\n  (cond ((null? records) false)\n        ((equal? key (caar records)) (car records))\n        (else (assoc key (cdr records)))))\n\n(define (make-table)\n  (let ((local-table (list '*table*)))\n    (define (lookup key-1 key-2)\n      (let ((subtable (assoc key-1 (cdr local-table))))\n        (if subtable\n            (let ((record (assoc key-2 (cdr subtable))))\n              (if record\n                  (cdr record)\n                  false))\n            false)))\n    (define (insert! key-1 key-2 value)\n      (let ((subtable (assoc key-1 (cdr local-table))))\n        (if subtable\n            (let ((record (assoc key-2 (cdr subtable))))\n              (if record\n                  (set-cdr! record value)\n                  (set-cdr! subtable\n                            (cons (cons key-2 value)\n                                  (cdr subtable)))))\n            (set-cdr! local-table\n                      (cons (list key-1\n                                  (cons key-2 value))\n                            (cdr local-table)))))\n      'ok)    \n    (define (dispatch m)\n      (cond ((eq? m 'lookup-proc) lookup)\n            ((eq? m 'insert-proc!) insert!)\n            (else (error \"Unknown operation -- TABLE\" m))))\n    dispatch))\n\n(define operation-table (make-table))\n(define get (operation-table 'lookup-proc))\n(define put (operation-table 'insert-proc!))\n")),(0,o.kt)("h2",{id:"exercise-273"},"Exercise 2.73"),(0,o.kt)("details",null,(0,o.kt)("summary",null,"Dependencies"),(0,o.kt)("div",null,(0,o.kt)("div",null,"This is the detailed content"))),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scheme",metastring:'title="Code from exercise 2.73"',title:'"Code',from:!0,exercise:!0,'2.73"':!0},"(define (deriv exp var)\n  (cond ((number? exp) 0)\n        ((variable? exp) (if (same-variable? exp var) 1 0))\n        ((sum? exp)\n         (make-sum (deriv (addend exp) var)\n                   (deriv (augend exp) var)))\n        ((product? exp)\n         (make-sum\n           (make-product (multiplier exp)\n                         (deriv (multiplicand exp) var))\n           (make-product (deriv (multiplier exp) var)\n                         (multiplicand exp))))\n        (else (error \"unknown expression type -- DERIV\" exp))))\n\n(define (deriv exp var)\n   (cond ((number? exp) 0)\n         ((variable? exp) (if (same-variable? exp var) 1 0))\n         (else ((get 'deriv (operator exp)) (operands exp)\n                                            var))))\n(define (operator exp) (car exp))\n(define (operands exp) (cdr exp))\n\n;: ((get (operator exp) 'deriv) (operands exp) var)\n")),(0,o.kt)("p",null,(0,o.kt)("strong",{parentName:"p"}," a) "),'\nBecause there aren\'t any "tags" we can look for on a number or variable. The procedure "operator" will not work because a number or variable will not be a list.'),(0,o.kt)("p",null,(0,o.kt)("strong",{parentName:"p"}," b) ")),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scheme"},";; Message passing\n(define (make-from-real-imag x y)\n  (define (dispatch op)\n    (cond ((eq? op 'real-part) x)\n          ((eq? op 'imag-part) y)\n          ((eq? op 'magnitude)\n           (sqrt (+ (square x) (square y))))\n          ((eq? op 'angle) (atan y x))\n          (else\n           (error \"Unknown op -- MAKE-FROM-REAL-IMAG\" op))))\n  dispatch)\n\n(define (apply-generic op arg) (arg op))\n\f\n")))}d.isMDXComponent=!0}}]);