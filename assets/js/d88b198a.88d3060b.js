"use strict";(self.webpackChunksicp_docu=self.webpackChunksicp_docu||[]).push([[207],{7474:function(e,t){t.Z={repo:"jugheadjones10/yet-another-SICP-website",repoId:"MDEwOlJlcG9zaXRvcnkzOTc3NzgwODE=",category:"General",categoryId:"DIC_kwDOF7Wcoc4B-2HB",mapping:"pathname",reactionsEnabled:"0",emitMetadata:"0",theme:"light"}},7412:function(e,t,n){n.r(t),n.d(t,{frontMatter:function(){return p},contentTitle:function(){return s},metadata:function(){return u},toc:function(){return l},default:function(){return b}});var r=n(7462),a=n(3366),o=(n(7294),n(3905)),i=n(7870),c=n(7474),d=["components"],p={},s="Exercise 2.7",u={unversionedId:"chapter-2/intro-to-data-abstraction/exercise-2.07",id:"chapter-2/intro-to-data-abstraction/exercise-2.07",isDocsHomePage:!1,title:"Exercise 2.7",description:"Dependencies",source:"@site/docs/chapter-2/intro-to-data-abstraction/exercise-2.07.mdx",sourceDirName:"chapter-2/intro-to-data-abstraction",slug:"/chapter-2/intro-to-data-abstraction/exercise-2.07",permalink:"/chapter-2/intro-to-data-abstraction/exercise-2.07",editUrl:"https://github.com/facebook/docusaurus/edit/master/website/docs/chapter-2/intro-to-data-abstraction/exercise-2.07.mdx",version:"current",frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"Exercise 2.6",permalink:"/chapter-2/intro-to-data-abstraction/exercise-2.06"},next:{title:"Exercise 2.8",permalink:"/chapter-2/intro-to-data-abstraction/exercise-2.08"}},l=[{value:"Dependencies",id:"dependencies",children:[]}],m={toc:l};function b(e){var t=e.components,n=(0,a.Z)(e,d);return(0,o.kt)("wrapper",(0,r.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"exercise-27"},"Exercise 2.7"),(0,o.kt)("h3",{id:"dependencies"},"Dependencies"),(0,o.kt)("p",null,"The following code will be needed as reference for exercises 2.7-2.16 (all the interval arithmetic questions)."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scheme"},"(define (add-interval x y)\n  (make-interval (+ (lower-bound x) (lower-bound y))\n                 (+ (upper-bound x) (upper-bound y))))\n\n(define (mul-interval x y)\n  (let ((p1 (* (lower-bound x) (lower-bound y)))\n        (p2 (* (lower-bound x) (upper-bound y)))\n        (p3 (* (upper-bound x) (lower-bound y)))\n        (p4 (* (upper-bound x) (upper-bound y))))\n    (make-interval (min p1 p2 p3 p4)\n                   (max p1 p2 p3 p4))))\n\n(define (div-interval x y)\n  (mul-interval x \n                (make-interval (/ 1.0 (upper-bound y))\n                               (/ 1.0 (lower-bound y)))))\n\n(define (make-interval a b) (cons a b))\n")),(0,o.kt)(i.d,{repo:c.Z.repo,repoId:c.Z.repoId,category:c.Z.category,categoryId:c.Z.categoryId,mapping:c.Z.mapping,reactionsEnabled:c.Z.reactionsEnabled,emitMetadata:c.Z.emitMetadata,theme:c.Z.theme,mdxType:"Giscus"}))}b.isMDXComponent=!0}}]);