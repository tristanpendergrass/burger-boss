var r={};!function(r){function n(r,n,t){return t.a=r,t.f=n,t}function t(r){return n(2,r,(function(n){return function(t){return r(n,t)}}))}function e(r){return n(3,r,(function(n){return function(t){return function(e){return r(n,t,e)}}}))}function u(r){return n(4,r,(function(n){return function(t){return function(e){return function(u){return r(n,t,e,u)}}}}))}function a(r){return n(5,r,(function(n){return function(t){return function(e){return function(u){return function(a){return r(n,t,e,u,a)}}}}}))}function i(r){return n(6,r,(function(n){return function(t){return function(e){return function(u){return function(a){return function(i){return r(n,t,e,u,a,i)}}}}}}))}function o(r,n,t){return 2===r.a?r.f(n,t):r(n)(t)}function f(r,n,t,e){return 3===r.a?r.f(n,t,e):r(n)(t)(e)}function c(r,n,t,e,u){return 4===r.a?r.f(n,t,e,u):r(n)(t)(e)(u)}function s(r,n,t,e,u,a){return 5===r.a?r.f(n,t,e,u,a):r(n)(t)(e)(u)(a)}function v(r,n,t,e,u,a,i){return 6===r.a?r.f(n,t,e,u,a,i):r(n)(t)(e)(u)(a)(i)}var l=e((function(r,n,t){for(var e=Array(r),u=0;r>u;u++)e[u]=t(n+u);return e})),b=t((function(r,n){for(var t=Array(r),e=0;r>e&&n.b;e++)t[e]=n.a,n=n.b;return t.length=e,m(t,n)}));function d(r){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+r+".md")}function g(r,n){for(var t,e=[],u=h(r,n,0,e);u&&(t=e.pop());u=h(t.a,t.b,0,e));return u}function h(r,n,t,e){if(r===n)return!0;if("object"!=typeof r||null===r||null===n)return"function"==typeof r&&d(5),!1;if(t>100)return e.push(m(r,n)),!0;for(var u in 0>r.$&&(r=yn(r),n=yn(n)),r)if(!h(r[u],n[u],t+1,e))return!1;return!0}function p(r,n,t){if("object"!=typeof r)return r===n?0:n>r?-1:1;if(void 0===r.$)return(t=p(r.a,n.a))||(t=p(r.b,n.b))?t:p(r.c,n.c);for(;r.b&&n.b&&!(t=p(r.a,n.a));r=r.b,n=n.b);return t||(r.b?1:n.b?-1:0)}var $=t((function(r,n){var t=p(r,n);return 0>t?jn:t?kn:xn}));function m(r,n){return{a:r,b:n}}function w(r,n,t){return{a:r,b:n,c:t}}function y(r,n){var t={};for(var e in r)t[e]=r[e];for(var e in n)t[e]=n[e];return t}var x={$:0};function k(r,n){return{$:1,a:r,b:n}}var j=t(k);function A(r){for(var n=x,t=r.length;t--;)n=k(r[t],n);return n}function B(r){for(var n=[];r.b;r=r.b)n.push(r.a);return n}var T=e((function(r,n,t){for(var e=[];n.b&&t.b;n=n.b,t=t.b)e.push(o(r,n.a,t.a));return A(e)})),S=t((function(r,n){return A(B(n).sort((function(n,t){return p(r(n),r(t))})))})),_=t((function(r,n){return r+n})),E=t((function(r,n){var t=n%r;return 0===r?d(11):t>0&&0>r||0>t&&r>0?t+r:t})),N=Math.ceil,L=Math.floor,F=Math.log,G=t((function(r,n){return n.join(r)}));function C(r){return r+""}function O(r){return{$:2,b:r}}var M=O((function(r){return"number"!=typeof r?nr("an INT",r):r>-2147483647&&2147483647>r&&(0|r)===r?_n(r):!isFinite(r)||r%1?nr("an INT",r):_n(r)})),J=O((function(r){return"boolean"==typeof r?_n(r):nr("a BOOL",r)})),U=O((function(r){return"number"==typeof r?_n(r):nr("a FLOAT",r)})),q=(O((function(r){return _n(r)})),O((function(r){return"string"==typeof r?_n(r):r instanceof String?_n(r+""):nr("a STRING",r)}))),D=t((function(r,n){return{$:6,d:r,b:n}}));function R(r,n){return{$:9,f:r,g:n}}var Y,I=t((function(r,n){return{$:10,b:n,h:r}})),z=t((function(r,n){return R(r,[n])})),K=e((function(r,n,t){return R(r,[n,t])})),P=a((function(r,n,t,e,u){return R(r,[n,t,e,u])})),X=i((function(r,n,t,e,u,a){return R(r,[n,t,e,u,a])})),W=n(7,Y=function(r,n,t,e,u,a,i){return R(r,[n,t,e,u,a,i])},(function(r){return function(n){return function(t){return function(e){return function(u){return function(a){return function(i){return Y(r,n,t,e,u,a,i)}}}}}}})),H=t((function(r,n){return V(r,n)}));function V(r,n){switch(r.$){case 2:return r.b(n);case 5:return null===n?_n(r.c):nr("null",n);case 3:return Z(n)?Q(r.b,n,A):nr("a LIST",n);case 4:return Z(n)?Q(r.b,n,rr):nr("an ARRAY",n);case 6:var t=r.d;if("object"!=typeof n||null===n||!(t in n))return nr("an OBJECT with a field named `"+t+"`",n);var e=V(r.b,n[t]);return it(e)?e:An(o(Tn,t,e.a));case 7:var u=r.e;return Z(n)?n.length>u?(e=V(r.b,n[u]),it(e)?e:An(o(Sn,u,e.a))):nr("a LONGER array. Need index "+u+" but only see "+n.length+" entries",n):nr("an ARRAY",n);case 8:if("object"!=typeof n||null===n||Z(n))return nr("an OBJECT",n);var a=x;for(var i in n)if(n.hasOwnProperty(i)){if(e=V(r.b,n[i]),!it(e))return An(o(Tn,i,e.a));a=k(m(i,e.a),a)}return _n(Dn(a));case 9:for(var f=r.f,c=r.g,s=0;c.length>s;s++){if(e=V(c[s],n),!it(e))return e;f=f(e.a)}return _n(f);case 10:return e=V(r.b,n),it(e)?V(r.h(e.a),n):e;case 11:for(var v=x,l=r.g;l.b;l=l.b){if(e=V(l.a,n),it(e))return e;v=k(e.a,v)}return An(En(Dn(v)));case 1:return An(o(Bn,r.a,n));case 0:return _n(r.a)}}function Q(r,n,t){for(var e=n.length,u=Array(e),a=0;e>a;a++){var i=V(r,n[a]);if(!it(i))return An(o(Sn,a,i.a));u[a]=i.a}return _n(t(u))}function Z(r){return Array.isArray(r)||"undefined"!=typeof FileList&&r instanceof FileList}function rr(r){return o(at,r.length,(function(n){return r[n]}))}function nr(r,n){return An(o(Bn,"Expecting "+r,n))}function tr(r,n){if(r===n)return!0;if(r.$!==n.$)return!1;switch(r.$){case 0:case 1:return r.a===n.a;case 2:return r.b===n.b;case 5:return r.c===n.c;case 3:case 4:case 8:return tr(r.b,n.b);case 6:return r.d===n.d&&tr(r.b,n.b);case 7:return r.e===n.e&&tr(r.b,n.b);case 9:return r.f===n.f&&er(r.g,n.g);case 10:return r.h===n.h&&tr(r.b,n.b);case 11:return er(r.g,n.g)}}function er(r,n){var t=r.length;if(t!==n.length)return!1;for(var e=0;t>e;e++)if(!tr(r[e],n[e]))return!1;return!0}function ur(r){return r}function ar(r){return{$:0,a:r}}function ir(r){return{$:2,b:r,c:null}}var or=t((function(r,n){return{$:3,b:r,d:n}})),fr=0;function cr(r){var n={$:0,e:fr++,f:r,g:null,h:[]};return gr(n),n}function sr(r){return ir((function(n){n(ar(cr(r)))}))}function vr(r,n){r.h.push(n),gr(r)}var lr=t((function(r,n){return ir((function(t){vr(r,n),t(ar(0))}))})),br=!1,dr=[];function gr(r){if(dr.push(r),!br){for(br=!0;r=dr.shift();)hr(r);br=!1}}function hr(r){for(;r.f;){var n=r.f.$;if(0===n||1===n){for(;r.g&&r.g.$!==n;)r.g=r.g.i;if(!r.g)return;r.f=r.g.b(r.f.a),r.g=r.g.i}else{if(2===n)return void(r.f.c=r.f.b((function(n){r.f=n,gr(r)})));if(5===n){if(0===r.h.length)return;r.f=r.f.b(r.h.shift())}else r.g={$:3===n?0:1,b:r.f.b,i:r.g},r.f=r.f.d}}}var pr={};function $r(r,n,t,e,u){return{b:r,c:n,d:t,e:e,f:u}}function mr(r,n){var t={g:n,h:void 0},e=r.c,u=r.d,a=r.e,i=r.f;return t.h=cr(o(or,(function r(n){return o(or,r,{$:5,b:function(r){var o=r.a;return 0===r.$?f(u,t,o,n):a&&i?c(e,t,o.i,o.j,n):f(e,t,a?o.i:o.j,n)}})}),r.b))}var wr=t((function(r,n){return ir((function(t){r.g(n),t(ar(0))}))})),yr=t((function(r,n){return o(lr,r.h,{$:0,a:n})}));function xr(r){return function(n){return{$:1,k:r,l:n}}}function kr(r){return{$:2,m:r}}var jr=[],Ar=!1;function Br(r,n,t){if(jr.push({p:r,q:n,r:t}),!Ar){Ar=!0;for(var e;e=jr.shift();)Tr(e.p,e.q,e.r);Ar=!1}}function Tr(r,n,t){var e={};for(var u in Sr(!0,n,e,null),Sr(!1,t,e,null),r)vr(r[u],{$:"fx",a:e[u]||{i:x,j:x}})}function Sr(r,n,t,e){switch(n.$){case 1:var u=n.k,a=(f=r,c=u,s=e,v=n.l,o(f?pr[c].e:pr[c].f,(function(r){for(var n=s;n;n=n.t)r=n.s(r);return r}),v));return void(t[u]=function(r,n,t){return t=t||{i:x,j:x},r?t.i=k(n,t.i):t.j=k(n,t.j),t}(r,a,t[u]));case 2:for(var i=n.m;i.b;i=i.b)Sr(r,i.a,t,e);return;case 3:return void Sr(r,n.o,t,{s:n.n,t:e})}var f,c,s,v}var _r,Er=t((function(r,n){return n})),Nr="undefined"!=typeof document?document:{};function Lr(r,n){r.appendChild(n)}function Fr(r){return{$:0,a:r}}var Gr=t((function(r,n){return t((function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b||0,u.push(i)}return a+=u.length,{$:1,c:n,d:qr(t),e:u,f:r,b:a}}))}))(void 0);t((function(r,n){return t((function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b.b||0,u.push(i)}return a+=u.length,{$:2,c:n,d:qr(t),e:u,f:r,b:a}}))}))(void 0);var Cr,Or=t((function(r,n){return{$:"a0",n:r,o:n}})),Mr=t((function(r,n){return{$:"a1",n:r,o:n}})),Jr=t((function(r,n){return{$:"a2",n:r,o:n}})),Ur=t((function(r,n){return{$:"a3",n:r,o:n}}));function qr(r){for(var n={};r.b;r=r.b){var t=r.a,e=t.$,u=t.n,a=t.o;if("a2"!==e){var i=n[e]||(n[e]={});"a3"===e&&"class"===u?Dr(i,u,a):i[u]=a}else"className"===u?Dr(n,u,a):n[u]=a}return n}function Dr(r,n,t){var e=r[n];r[n]=e?e+" "+t:t}function Rr(r,n){var t=r.$;if(5===t)return Rr(r.k||(r.k=r.m()),n);if(0===t)return Nr.createTextNode(r.a);if(4===t){for(var e=r.k,u=r.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var a={j:u,p:n};return(i=Rr(e,a)).elm_event_node_ref=a,i}if(3===t)return Yr(i=r.h(r.g),n,r.d),i;var i=r.f?Nr.createElementNS(r.f,r.c):Nr.createElement(r.c);_r&&"a"==r.c&&i.addEventListener("click",_r(i)),Yr(i,n,r.d);for(var o=r.e,f=0;o.length>f;f++)Lr(i,Rr(1===t?o[f]:o[f].b,n));return i}function Yr(r,n,t){for(var e in t){var u=t[e];"a1"===e?Ir(r,u):"a0"===e?Pr(r,n,u):"a3"===e?zr(r,u):"a4"===e?Kr(r,u):("value"!==e&&"checked"!==e||r[e]!==u)&&(r[e]=u)}}function Ir(r,n){var t=r.style;for(var e in n)t[e]=n[e]}function zr(r,n){for(var t in n){var e=n[t];void 0!==e?r.setAttribute(t,e):r.removeAttribute(t)}}function Kr(r,n){for(var t in n){var e=n[t],u=e.f,a=e.o;void 0!==a?r.setAttributeNS(u,t,a):r.removeAttributeNS(u,t)}}function Pr(r,n,t){var e=r.elmFs||(r.elmFs={});for(var u in t){var a=t[u],i=e[u];if(a){if(i){if(i.q.$===a.$){i.q=a;continue}r.removeEventListener(u,i)}i=Xr(n,a),r.addEventListener(u,i,Cr&&{passive:2>st(a)}),e[u]=i}else r.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Cr=!0}}))}catch(Y){}function Xr(r,n){function t(n){var e=t.q,u=V(e.a,n);if(it(u)){for(var a,i=st(e),o=u.a,f=i?3>i?o.a:o.o:o,c=1==i?o.b:3==i&&o.aU,s=(c&&n.stopPropagation(),(2==i?o.b:3==i&&o.aT)&&n.preventDefault(),r);a=s.j;){if("function"==typeof a)f=a(f);else for(var v=a.length;v--;)f=a[v](f);s=s.p}s(f,c)}}return t.q=n,t}function Wr(r,n){return r.$==n.$&&tr(r.a,n.a)}function Hr(r,n,t,e){var u={$:n,r:t,s:e,t:void 0,u:void 0};return r.push(u),u}function Vr(r,n,t,e){if(r!==n){var u=r.$,a=n.$;if(u!==a){if(1!==u||2!==a)return void Hr(t,0,e,n);n=function(r){for(var n=r.e,t=n.length,e=Array(t),u=0;t>u;u++)e[u]=n[u].b;return{$:1,c:r.c,d:r.d,e:e,f:r.f,b:r.b}}(n),a=1}switch(a){case 5:for(var i=r.l,o=n.l,f=i.length,c=f===o.length;c&&f--;)c=i[f]===o[f];if(c)return void(n.k=r.k);n.k=n.m();var s=[];return Vr(r.k,n.k,s,0),void(s.length>0&&Hr(t,1,e,s));case 4:for(var v=r.j,l=n.j,b=!1,d=r.k;4===d.$;)b=!0,"object"!=typeof v?v=[v,d.j]:v.push(d.j),d=d.k;for(var g=n.k;4===g.$;)b=!0,"object"!=typeof l?l=[l,g.j]:l.push(g.j),g=g.k;return b&&v.length!==l.length?void Hr(t,0,e,n):((b?function(r,n){for(var t=0;r.length>t;t++)if(r[t]!==n[t])return!1;return!0}(v,l):v===l)||Hr(t,2,e,l),void Vr(d,g,t,e+1));case 0:return void(r.a!==n.a&&Hr(t,3,e,n.a));case 1:return void Qr(r,n,t,e,rn);case 2:return void Qr(r,n,t,e,nn);case 3:if(r.h!==n.h)return void Hr(t,0,e,n);var h=Zr(r.d,n.d);h&&Hr(t,4,e,h);var p=n.i(r.g,n.g);return void(p&&Hr(t,5,e,p))}}}function Qr(r,n,t,e,u){if(r.c===n.c&&r.f===n.f){var a=Zr(r.d,n.d);a&&Hr(t,4,e,a),u(r,n,t,e)}else Hr(t,0,e,n)}function Zr(r,n,t){var e;for(var u in r)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in n){var a=r[u],i=n[u];a===i&&"value"!==u&&"checked"!==u||"a0"===t&&Wr(a,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:r[u].f,o:void 0}:"string"==typeof r[u]?"":null;else{var o=Zr(r[u],n[u]||{},u);o&&((e=e||{})[u]=o)}for(var f in n)f in r||((e=e||{})[f]=n[f]);return e}function rn(r,n,t,e){var u=r.e,a=n.e,i=u.length,o=a.length;i>o?Hr(t,6,e,{v:o,i:i-o}):o>i&&Hr(t,7,e,{v:i,e:a});for(var f=o>i?i:o,c=0;f>c;c++){var s=u[c];Vr(s,a[c],t,++e),e+=s.b||0}}function nn(r,n,t,e){for(var u=[],a={},i=[],o=r.e,f=n.e,c=o.length,s=f.length,v=0,l=0,b=e;c>v&&s>l;){var d=(B=o[v]).a,g=(T=f[l]).a,h=B.b,p=T.b,$=void 0,m=void 0;if(d!==g){var w=o[v+1],y=f[l+1];if(w){var x=w.a,k=w.b;m=g===x}if(y){var j=y.a,A=y.b;$=d===j}if($&&m)Vr(h,A,u,++b),en(a,u,d,p,l,i),b+=h.b||0,un(a,u,d,k,++b),b+=k.b||0,v+=2,l+=2;else if($)b++,en(a,u,g,p,l,i),Vr(h,A,u,b),b+=h.b||0,v+=1,l+=2;else if(m)un(a,u,d,h,++b),b+=h.b||0,Vr(k,p,u,++b),b+=k.b||0,v+=2,l+=1;else{if(!w||x!==j)break;un(a,u,d,h,++b),en(a,u,g,p,l,i),b+=h.b||0,Vr(k,A,u,++b),b+=k.b||0,v+=2,l+=2}}else Vr(h,p,u,++b),b+=h.b||0,v++,l++}for(;c>v;){var B;b++,un(a,u,(B=o[v]).a,h=B.b,b),b+=h.b||0,v++}for(;s>l;){var T,S=S||[];en(a,u,(T=f[l]).a,T.b,void 0,S),l++}(u.length>0||i.length>0||S)&&Hr(t,8,e,{w:u,x:i,y:S})}var tn="_elmW6BL";function en(r,n,t,e,u,a){var i=r[t];if(!i)return a.push({r:u,A:i={c:0,z:e,r:u,s:void 0}}),void(r[t]=i);if(1===i.c){a.push({r:u,A:i}),i.c=2;var o=[];return Vr(i.z,e,o,i.r),i.r=u,void(i.s.s={w:o,A:i})}en(r,n,t+tn,e,u,a)}function un(r,n,t,e,u){var a=r[t];if(a){if(0===a.c){a.c=2;var i=[];return Vr(e,a.z,i,u),void Hr(n,9,u,{w:i,A:a})}un(r,n,t+tn,e,u)}else{var o=Hr(n,9,u,void 0);r[t]={c:1,z:e,r:u,s:o}}}function an(r,n,t,e){on(r,n,t,0,0,n.b,e)}function on(r,n,t,e,u,a,i){for(var o=t[e],f=o.r;f===u;){var c=o.$;if(1===c)an(r,n.k,o.s,i);else if(8===c)o.t=r,o.u=i,(s=o.s.w).length>0&&on(r,n,s,0,u,a,i);else if(9===c){o.t=r,o.u=i;var s,v=o.s;v&&(v.A.s=r,(s=v.w).length>0&&on(r,n,s,0,u,a,i))}else o.t=r,o.u=i;if(!(o=t[++e])||(f=o.r)>a)return e}var l=n.$;if(4===l){for(var b=n.k;4===b.$;)b=b.k;return on(r,b,t,e,u+1,a,r.elm_event_node_ref)}for(var d=n.e,g=r.childNodes,h=0;d.length>h;h++){u++;var p=1===l?d[h]:d[h].b,$=u+(p.b||0);if(!(u>f||f>$||(o=t[e=on(g[h],p,t,e,u,$,i)])&&(f=o.r)<=a))return e;u=$}return e}function fn(r,n){for(var t=0;n.length>t;t++){var e=n[t],u=e.t,a=cn(u,e);u===r&&(r=a)}return r}function cn(r,n){switch(n.$){case 0:return function(r,n,t){var e=r.parentNode,u=Rr(n,t);return u.elm_event_node_ref||(u.elm_event_node_ref=r.elm_event_node_ref),e&&u!==r&&e.replaceChild(u,r),u}(r,n.s,n.u);case 4:return Yr(r,n.u,n.s),r;case 3:return r.replaceData(0,r.length,n.s),r;case 1:return fn(r,n.s);case 2:return r.elm_event_node_ref?r.elm_event_node_ref.j=n.s:r.elm_event_node_ref={j:n.s,p:n.u},r;case 6:for(var t=n.s,e=0;t.i>e;e++)r.removeChild(r.childNodes[t.v]);return r;case 7:for(var u=(t=n.s).e,a=r.childNodes[e=t.v];u.length>e;e++)r.insertBefore(Rr(u[e],n.u),a);return r;case 9:if(!(t=n.s))return r.parentNode.removeChild(r),r;var i=t.A;return void 0!==i.r&&r.parentNode.removeChild(r),i.s=fn(r,t.w),r;case 8:return function(r,n){var t=n.s,e=function(r,n){if(r){for(var t=Nr.createDocumentFragment(),e=0;r.length>e;e++){var u=r[e].A;Lr(t,2===u.c?u.s:Rr(u.z,n.u))}return t}}(t.y,n);r=fn(r,t.w);for(var u=t.x,a=0;u.length>a;a++){var i=u[a],o=i.A,f=2===o.c?o.s:Rr(o.z,n.u);r.insertBefore(f,r.childNodes[i.r])}return e&&Lr(r,e),r}(r,n);case 5:return n.s(r);default:d(10)}}function sn(r){if(3===r.nodeType)return Fr(r.textContent);if(1!==r.nodeType)return Fr("");for(var n=x,t=r.attributes,e=t.length;e--;){var u=t[e];n=k(o(Ur,u.name,u.value),n)}var a=r.tagName.toLowerCase(),i=x,c=r.childNodes;for(e=c.length;e--;)i=k(sn(c[e]),i);return f(Gr,a,n,i)}var vn=u((function(r,n,t,e){return function(n,t,u,a,i,f){var c=o(H,n,t?t.flags:void 0);it(c)||d(2);var s={},v=u(c.a),l=v.a,b=function(n,t){var u=r.bl,a=e.node,i=sn(a);return function(r,n){n(r);var t=0;function e(){t=1===t?0:(bn(e),n(r),1)}return function(u,a){r=u,a?(n(r),2===t&&(t=1)):(0===t&&bn(e),t=2)}}(t,(function(r){var t,e=u(r),o=(Vr(i,e,t=[],0),t);a=function(r,n,t,e){return 0===t.length?r:(an(r,n,t,e),fn(r,t))}(a,i,o,n),i=e}))}(h,l),g=function(r,n){var t;for(var e in pr){var u=pr[e];u.a&&((t=t||{})[e]=u.a(e,n)),r[e]=mr(u,n)}return t}(s,h);function h(r,n){var t=o(a,r,l);b(l=t.a,n),Br(s,t.b,i(l))}return Br(s,v.b,i(l)),g?{ports:g}:{}}(n,e,r.be,r.bk,r.bj)})),ln="undefined"!=typeof cancelAnimationFrame?cancelAnimationFrame:function(r){clearTimeout(r)},bn="undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(r){return setTimeout(r,1e3/60)},dn={addEventListener:function(){},removeEventListener:function(){}},gn="undefined"!=typeof document?document:dn,hn="undefined"!=typeof window?window:dn,pn=e((function(r,n,t){return sr(ir((function(){function e(r){cr(t(r))}return r.addEventListener(n,e,Cr&&{passive:!0}),function(){r.removeEventListener(n,e)}})))})),$n=t((function(r,n){var t=V(r,n);return it(t)?Ln(t.a):Fn})),mn=j,wn=e((function(r,n,t){for(;;){if(-2===t.$)return n;var e=t.d,u=r,a=f(r,t.b,t.c,f(wn,r,n,t.e));r=u,n=a,t=e}})),yn=function(r){return f(wn,e((function(r,n,t){return o(mn,m(r,n),t)})),x,r)},xn=1,kn=2,jn=0,An=function(r){return{$:1,a:r}},Bn=t((function(r,n){return{$:3,a:r,b:n}})),Tn=t((function(r,n){return{$:0,a:r,b:n}})),Sn=t((function(r,n){return{$:1,a:r,b:n}})),_n=function(r){return{$:0,a:r}},En=function(r){return{$:2,a:r}},Nn=_,Ln=function(r){return{$:0,a:r}},Fn={$:1},Gn=C,Cn=t((function(r,n){return o(G,r,B(n))})),On=e((function(r,n,t){for(;;){if(!t.b)return n;var e=t.b,u=r,a=o(r,t.a,n);r=u,n=a,t=e}})),Mn=T,Jn=e((function(r,n,t){for(;;){if(p(r,n)>=1)return t;var e=r,u=n-1,a=o(mn,n,t);r=e,n=u,t=a}})),Un=t((function(r,n){return f(Jn,r,n,x)})),qn=t((function(r,n){return f(Mn,r,o(Un,0,(e=n,f(On,t((function(r,n){return n+1})),0,e)-1)),n);var e})),Dn=function(r){return f(On,mn,x,r)},Rn=32,Yn=u((function(r,n,t,e){return{$:0,a:r,b:n,c:t,d:e}})),In=[],zn=N,Kn=t((function(r,n){return F(n)/F(r)})),Pn=zn(o(Kn,2,Rn)),Xn=c(Yn,0,Pn,In,In),Wn=l,Hn=L,Vn=function(r){return r.length},Qn=t((function(r,n){return p(r,n)>0?r:n})),Zn=b,rt=t((function(r,n){for(;;){var t=o(Zn,Rn,r),e=t.b,u=o(mn,{$:0,a:t.a},n);if(!e.b)return Dn(u);r=e,n=u}})),nt=function(r){return r.a},tt=t((function(r,n){for(;;){var t=zn(n/Rn);if(1===t)return o(Zn,Rn,r).a;r=o(rt,r,x),n=t}})),et=t((function(r,n){if(n.g){var t=n.g*Rn,e=Hn(o(Kn,Rn,t-1)),u=r?Dn(n.k):n.k,a=o(tt,u,n.g);return c(Yn,Vn(n.i)+t,o(Qn,5,e*Pn),a,n.i)}return c(Yn,Vn(n.i),Pn,In,n.i)})),ut=a((function(r,n,t,e,u){for(;;){if(0>n)return o(et,!1,{k:e,g:t/Rn|0,i:u});var a={$:1,a:f(Wn,Rn,n,r)};n-=Rn,e=o(mn,a,e)}})),at=t((function(r,n){if(r>0){var t=r%Rn;return s(ut,n,r-t-Rn,r,x,f(Wn,t,r-t,n))}return Xn})),it=function(r){return!r.$},ot=I,ft=z,ct=K,st=function(r){switch(r.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},vt=function(r){return r},lt=ar,bt=lt(0),dt=u((function(r,n,t,e){if(e.b){var u=e.a,a=e.b;if(a.b){var i=a.a,s=a.b;if(s.b){var v=s.a,l=s.b;if(l.b){var b=l.b;return o(r,u,o(r,i,o(r,v,o(r,l.a,t>500?f(On,r,n,Dn(b)):c(dt,r,n,t+1,b)))))}return o(r,u,o(r,i,o(r,v,n)))}return o(r,u,o(r,i,n))}return o(r,u,n)}return n})),gt=e((function(r,n,t){return c(dt,r,n,0,t)})),ht=t((function(r,n){return f(gt,t((function(n,t){return o(mn,r(n),t)})),x,n)})),pt=or,$t=t((function(r,n){return o(pt,(function(n){return lt(r(n))}),n)})),mt=e((function(r,n,t){return o(pt,(function(n){return o(pt,(function(t){return lt(o(r,n,t))}),t)}),n)})),wt=function(r){return f(gt,mt(mn),lt(x),r)},yt=wr,xt=t((function(r,n){var t=n;return sr(o(pt,yt(r),t))}));pr.Task=$r(bt,e((function(r,n){return o($t,(function(){return 0}),wt(o(ht,xt(r),n)))})),e((function(){return lt(0)})),t((function(r,n){return o($t,r,n)}))),xr("Task");var kt=vn,jt=D,At={$:0},Bt=t((function(r,n){return{$:0,a:r,b:n}})),Tt=function(r){var n=r.b;return o(Bt,1664525*r.a+n>>>0,n)},St=function(r){var n=Tt(o(Bt,0,1013904223));return Tt(o(Bt,n.a+r>>>0,n.b))},_t=kr(x),Et=M,Nt=function(r){return{$:2,a:r}},Lt=kr,Ft={$:6},Gt={$:0},Ct=t((function(r,n){return n.$||1!==n.a.$?Gt:Ft})),Ot={$:7},Mt=function(r){return{$:4,a:r}},Jt=function(r){return{$:3,a:r}},Ut=function(r){return{$:5,a:r}},qt=e((function(r,n,t){return n(r(t))})),Dt=t((function(r,n){r:for(;;){if(r>0){if(n.b){r-=1,n=n.b;continue r}return n}return n}})),Rt=function(r){return r.b?Ln(r.a):Fn},Yt=function(r){return o(qt,Dt(r),Rt)},It=t((function(r,n){var t=r.a;if(1!==t.$)return Gt;if(1===n.$)return Gt;if(n.a.$)return Ot;var e=n.a.a,u=o(Yt,e,t.a.d);if(1===u.$)return Gt;switch(u.a.$){case 0:return Gt;case 1:return Jt(e);case 2:return Mt(e);default:return Ut(e)}})),zt=Lt(x),Kt=function(r){return{$:1,a:r}},Pt=e((function(r,n,t){return{U:t,af:n,ag:r}})),Xt=lt(f(Pt,x,Fn,0)),Wt=function(r){return ir((function(n){var t=r.f;2===t.$&&t.c&&t.c(),r.f=null,n(ar(0))}))},Ht=ir((function(r){r(ar(Date.now()))})),Vt=ir((function(r){var n=bn((function(){r(ar(Date.now()))}));return function(){ln(n)}})),Qt=yr,Zt=sr,re=e((function(r,n,t){var e=t.U,u=t.af,a=m(u,n);return 1===a.a.$?a.b.b?o(pt,(function(r){return o(pt,(function(t){return lt(f(Pt,n,Ln(r),t))}),Ht)}),Zt(o(pt,Qt(r),Vt))):Xt:a.b.b?lt(f(Pt,n,u,e)):o(pt,(function(){return Xt}),Wt(a.a.a))})),ne=vt,te=e((function(r,n,t){var e=t.U,u=t.ag,a=function(t){return o(yt,r,t.a(t.$?n-e:ne(n)))};return o(pt,(function(r){return o(pt,(function(){return lt(f(Pt,u,Ln(r),n))}),wt(o(ht,a,u)))}),Zt(o(pt,Qt(r),Vt)))})),ee=e((function(r,n,t){return r(n(t))}));pr["Browser.AnimationManager"]=$r(Xt,re,te,0,t((function(r,n){return n.$?Kt(o(ee,r,n.a)):{$:0,a:o(ee,r,n.a)}})));var ue=xr("Browser.AnimationManager"),ae=e((function(r,n,t){return{$:0,a:r,b:n,c:t}})),ie=t((function(r,n){return{ac:n,ag:r}})),oe={$:-2},fe=oe,ce=lt(o(ie,x,fe)),se=function(r){return m(function(r,n){if("string"==typeof r)return r+n;if(!r.b)return n;var t=k(r.a,n);r=r.b;for(var e=t;r.b;r=r.b)e=e.b=k(r.a,n);return t}(r.a?"w_":"d_",r.b),r)},ve=a((function(r,n,t,e,u){return{$:-1,a:r,b:n,c:t,d:e,e:u}})),le=a((function(r,n,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return s(ve,r,n,t,e,u);var a=e.d;return i=e.e,s(ve,0,e.b,e.c,s(ve,1,a.b,a.c,a.d,a.e),s(ve,1,n,t,i,u))}var i,o=u.b,f=u.c,c=u.d,v=u.e;return-1!==e.$||e.a?s(ve,r,o,f,s(ve,0,n,t,e,c),v):s(ve,0,n,t,s(ve,1,e.b,e.c,e.d,i=e.e),s(ve,1,o,f,c,v))})),be=$,de=e((function(r,n,t){if(-2===t.$)return s(ve,0,r,n,oe,oe);var e=t.a,u=t.b,a=t.c,i=t.d,c=t.e;switch(o(be,r,u)){case 0:return s(le,e,u,a,f(de,r,n,i),c);case 1:return s(ve,e,u,n,i,c);default:return s(le,e,u,a,i,f(de,r,n,c))}})),ge=e((function(r,n,t){var e=f(de,r,n,t);return-1!==e.$||e.a?e:s(ve,1,e.b,e.c,e.d,e.e)})),he=function(r){return f(On,t((function(r,n){return f(ge,r.a,r.b,n)})),fe,r)},pe=e((function(r,n,t){for(;;){if(-2===t.$)return n;var e=t.e,u=r,a=f(r,t.b,t.c,f(pe,r,n,t.d));r=u,n=a,t=e}})),$e=i((function(r,n,u,a,i,o){var s=f(pe,e((function(t,e,a){r:for(;;){var i=a.a,o=a.b;if(i.b){var s=i.a,v=s.a,l=s.b,b=i.b;if(0>p(v,t)){a=m(b,f(r,v,l,o));continue r}return p(v,t)>0?m(i,f(u,t,e,o)):m(b,c(n,v,l,e,o))}return m(i,f(u,t,e,o))}})),m(yn(a),o),i),v=s.a,l=s.b;return f(On,t((function(n,t){return f(r,n.a,n.b,t)})),l,v)})),me=t((function(r,n){return{aV:n,aR:r}})),we=e((function(r,n,t){return o($t,(function(r){return m(n,r)}),f(pn,t.a?hn:gn,t.b,(function(t){return o(Qt,r,o(me,n,t))})))})),ye=t((function(r,n){return f(pe,ge,n,r)})),xe=e((function(r,n,t){var a=e((function(n,t,e){var u=e.c;return w(e.a,e.b,o(mn,f(we,r,n,t),u))})),i=e((function(r,n,t){var e=t.b,u=t.c;return w(o(mn,n,t.a),e,u)})),c=u((function(r,n,t,e){var u=e.c;return w(e.a,f(ge,r,n,e.b),u)})),s=o(ht,se,n),l=v($e,i,c,a,t.ac,he(s),w(x,fe,x)),b=l.b,d=l.c;return o(pt,(function(r){return lt(o(ie,s,o(ye,b,he(r))))}),o(pt,(function(){return wt(d)}),wt(o(ht,Wt,l.a))))})),ke=e((function(r,n,t){var e=r(n);return e.$?t:o(mn,e.a,t)})),je=t((function(r,n){return f(gt,ke(r),x,n)}));pr["Browser.Events"]=$r(ce,xe,e((function(r,n,t){var e=n.aV,u=n.aR,a=o(je,(function(r){var n=r.b.c;return g(r.a,u)?o($n,n,e):Fn}),t.ag);return o(pt,(function(){return lt(t)}),wt(o(ht,yt(r),a)))})),0,t((function(r,n){return f(ae,n.a,n.b,o(ft,r,n.c))})));var Ae,Be,Te,Se=xr("Browser.Events"),_e=e((function(r,n,t){return Se(f(ae,r,n,t))})),Ee=o(_e,0,"keydown"),Ne=o(_e,0,"keyup"),Le=q,Fe={$:1},Ge=function(r){return{$:0,a:r}},Ce=function(r){switch(r){case"1":return Ln(Ge(0));case"2":return Ln(Ge(1));case"3":return Ln(Ge(2));case"4":return Ln(Ge(3));case"5":return Ln(Ge(4));case" ":return Ln(Fe);default:return Fn}},Oe=t((function(r,n){return{$:2,a:r,b:n}})),Me=function(r){return{$:2,a:r}},Je=function(r){return{$:1,a:r}},Ue=t((function(r,n){return{$:0,a:r,b:n}})),qe=e((function(r,n,t){var e=n,u=t;return function(n){var t=e(n),a=t.a,i=u(t.b),f=i.b;return m(o(r,a,i.a),f)}})),De=function(r){return m(1,r)},Re=function(r){return 0>r?-r:r},Ye=function(r){var n=r.a,t=277803737*(n^n>>>4+(n>>>28));return(t>>>22^t)>>>0},Ie=t((function(r,n){return function(t){var e=Tt(t),u=Re(n-r),a=Ye(e);return m((1*(67108863&Ye(t))*134217728+1*(134217727&a))/9007199254740992*u+r,Tt(e))}})),ze=e((function(r,n,t){for(;;){var e=r.a,u=r.b;if(!n.b)return u;var a=n.a,i=n.b;if(1>p(t,Re(e)))return u;r=a,n=i,t-=Re(e)}})),Ke=t((function(r,n){var t=n;return function(n){var e=t(n),u=e.b;return m(r(e.a),u)}})),Pe=t((function(r,n){var t=function(r){return Re(r.a)},e=t(r)+f(On,Nn,0,o(ht,t,n));return o(Ke,o(ze,r,n),o(Ie,0,e))})),Xe=t((function(r,n){return o(Pe,De(r),o(ht,De,n))})),We=f(qe,Ue,o(Xe,1e3,A([1200,1400,1600,1800,2e3])),o(Xe,0,A([1,2]))),He=u((function(r,n,t,e){for(;;){if(1>n)return m(r,e);var u=t(e),a=u.b;r=o(mn,u.a,r),n-=1,e=a}})),Ve=o(Ke,(function(r){return{x:6e4,G:!1,B:0,d:r}}),o(t((function(r,n){var t=n;return function(n){return c(He,x,r,t,n)}})),5,We)),Qe=function(r){switch(r){case 0:return m(50,60);case 1:return m(60,80);default:return m(80,95)}},Ze=t((function(r,n){var t=Hn(100*n),e=Qe(r),u=e.b;return p(t,e.a-8)>-1&&0>p(t,u)?1:0})),ru={$:3},nu=e((function(r,n,t){switch(t.$){case 2:var e=t.a,u=n/12e3,a=t.b+(r?6*u:u);return a>1?ru:o(Oe,e,a);case 0:e=t.b;var i=t.a-n;return i>0?o(Ue,i,e):{$:1,a:e};default:return t}})),tu=t((function(r,n){return r(n)})),eu=ur,uu=t((function(r,n){return f(On,(e=r,t((function(r,n){return n.push(e(r)),n}))),[],n);var e})),au=(Be=vt,function(r){pr[r]&&d(3)}(Ae="saveScores"),pr[Ae]={e:Er,u:Be,a:function(r){var n=[],t=pr[r].u,u=ir((function(r){var n=setTimeout((function(){r(ar(0))}),0);return function(){clearTimeout(n)}}));return pr[r].b=u,pr[r].c=e((function(r,e){for(;e.b;e=e.b)for(var a=n,i=t(e.a),o=0;a.length>o;o++)a[o](i);return u})),{subscribe:function(r){n.push(r)},unsubscribe:function(r){var t=(n=n.slice()).indexOf(r);0>t||n.splice(t,1)}}}},xr(Ae)),iu=t((function(r,n){var t=o(mn,r,n);return m(t,au(o(uu,eu,t)))})),ou=t((function(r,n){return qn(t((function(t,e){return g(t,n)?r(e):e})))})),fu=t((function(r,n){var t=m(n,_t);switch(r.$){case 0:return t;case 1:return m(y(n,{r:r.a}),_t);case 2:var e=r.a,u=n.a;if(1===u.$){if((B=u.a).x-e>0)return m(y(n,{a:Je(y(B,{x:B.x-e,d:o(ht,o(nu,B.G,e),B.d)}))}),_t);var a=o(iu,B.B,n.F),i=a.b;return m(y(n,{F:a.a,a:Me(B.B)}),i)}return t;case 10:var c=r.a,s=n.a;if(1===s.$){var v=o(Yt,c,(B=s.a).d);r:for(;!v.$;)switch(v.a.$){case 1:var l=v.a.a;return m(y(n,{a:Je(y(B,{d:f(ou,(function(){return o(Oe,l,0)}),c,B.d)}))}),_t);case 2:var b=v.a,d=(l=b.a,b.b),g=o(tu,We,n.r),h=g.a;return m(y(n,{r:g.b,a:Je(y(B,{B:B.B+o(Ze,l,d),d:f(ou,(function(){return h}),c,B.d)}))}),_t);case 3:var p=o(tu,We,n.r);return h=p.a,m(y(n,{r:p.b,a:Je(y(B,{d:f(ou,(function(){return h}),c,B.d)}))}),_t);default:break r}return t}return t;case 3:c=r.a;var $=n.a;return 1===$.$?m(y(n,{a:Je(y(B=$.a,{d:f(ou,(function(r){return 1===r.$?o(Oe,r.a,0):r}),c,B.d)}))}),_t):t;case 4:c=r.a;var w=n.a;if(1===w.$){var x=o(Yt,c,(B=w.a).d);if(x.$||2!==x.a.$)return t;var k=x.a,j=(l=k.a,d=k.b,o(tu,We,n.r));return h=j.a,m(y(n,{r:j.b,a:Je(y(B,{B:B.B+o(Ze,l,d),d:f(ou,(function(){return h}),c,B.d)}))}),_t)}return t;case 5:c=r.a;var A=n.a;if(1===A.$){var B=A.a,T=o(tu,We,n.r);return h=T.a,m(y(n,{r:T.b,a:Je(y(B,{d:f(ou,(function(r){return 3===r.$?h:r}),c,B.d)}))}),_t)}return t;case 7:var S=n.a;return 1===S.$?m(y(n,{a:Je(y(B=S.a,{G:!1}))}),_t):t;case 6:var _=n.a;return 1===_.$?m(y(n,{a:Je(y(B=_.a,{G:!0}))}),_t):t;case 8:if(n.a.$)return t;var E=o(tu,Ve,n.r);return m(y(n,{r:E.b,a:Je(B=E.a)}),_t);case 9:return m(y(n,{a:At}),_t);case 11:return m(y(n,{aL:!0}),_t);case 12:return m(y(n,{aL:!1}),_t);default:var N=n.a;if(1===N.$){var L=o(iu,(B=N.a).B,n.F);return i=L.b,m(y(n,{F:L.a,a:Me(B.B)}),i)}return t}})),cu={$:13},su={$:11},vu={$:9},lu={$:8},bu=t((function(r,n){return n.b?f(gt,mn,n,r):r})),du=Gr("button"),gu=ur,hu=t((function(r,n){return o(Jr,r,gu(n))})),pu=hu("className"),$u=pu("py-1 px-2 bg-gray-100 hover:bg-gray-200 active:bg-gray-300 border border-black rounded-sm text-sm select-none"),mu=t((function(r,n){return f(gt,t((function(n,t){return r(n)?o(mn,n,t):t})),x,n)})),wu=function(r){return r.b},yu=function(r){return pu(o(Cn," ",o(ht,nt,o(mu,wu,r))))},xu=pu("flex flex-col items-center gap-2"),ku=Gr("div"),ju=E,Au=function(r){return!o(ju,2,Hn(r/100))},Bu=Gr("img"),Tu=function(r){var n;return o(hu,"src",/^\s*(javascript:|data:text\/html)/i.test(n=r)?"":n)},Su=function(r){var n=pu("w-auto h-full");return o(ku,A([pu("h-full"),yu(A([m("hidden",!r.G)])),pu("bg-gradient-to-t from-yellow-300 to-transparent")]),A([o(Bu,A([Tu("Jets11.png"),n,yu(A([m("hidden",Au(r.x))]))]),x),o(Bu,A([Tu("Jets12.png"),n,yu(A([m("hidden",!Au(r.x))]))]),x)]))},_u=function(r){var n=pu("w-auto h-full");return o(ku,A([pu("h-full"),yu(A([m("hidden",!r.G)])),pu("bg-gradient-to-t from-yellow-300 to-transparent")]),A([o(Bu,A([Tu("Jets11.png"),n,yu(A([m("hidden",!Au(r.x))]))]),x),o(Bu,A([Tu("Jets12.png"),n,yu(A([m("hidden",Au(r.x))]))]),x)]))},Eu=Gr("h2"),Nu=Gr("hr"),Lu={aT:!0,aU:!1},Fu=Or,Gu=t((function(r,n){return o(Fu,r,{$:3,a:n})})),Cu=a((function(r,n,t,e,u){return{an:u,ax:e,aF:n,aG:t,aH:r}})),Ou=J,Mu=U,Ju=X,Uu=v(Ju,a((function(r,n,t,e,u){return{c:n,aI:t,aN:e,aO:u,e:r}})),o(jt,"width",Mu),o(jt,"height",Mu),o(jt,"pressure",Mu),o(jt,"tiltX",Mu),o(jt,"tiltY",Mu)),qu=i((function(r,n,t,e,u,a){return{ak:n,am:t,ay:r,aD:e,aE:u,aJ:a}})),Du=o(ft,(function(r){switch(r){case 0:return 1;case 1:return 2;case 2:return 3;case 3:return 4;case 4:return 5;default:return 0}}),o(jt,"button",Et)),Ru=f(ct,t((function(r,n){return m(r,n)})),o(jt,"clientX",Mu),o(jt,"clientY",Mu)),Yu=function(r,n,t,e,u,a,i,o){return 7===r.a?r.f(n,t,e,u,a,i,o):r(n)(t)(e)(u)(a)(i)(o)}(W,qu,s(P,u((function(r,n,t,e){return{ai:r,ao:n,aA:t,aK:e}})),o(jt,"altKey",Ou),o(jt,"ctrlKey",Ou),o(jt,"metaKey",Ou),o(jt,"shiftKey",Ou)),Du,Ru,f(ct,t((function(r,n){return m(r,n)})),o(jt,"offsetX",Mu),o(jt,"offsetY",Mu)),f(ct,t((function(r,n){return m(r,n)})),o(jt,"pageX",Mu),o(jt,"pageY",Mu)),f(ct,t((function(r,n){return m(r,n)})),o(jt,"screenX",Mu),o(jt,"screenY",Mu))),Iu=v(Ju,Cu,o(jt,"pointerType",o(ft,(function(r){switch(r){case"pen":return 2;case"touch":return 1;default:return 0}}),Le)),Yu,o(jt,"pointerId",Et),o(jt,"isPrimary",Ou),Uu),zu=e((function(r,n,t){return o(Gu,r,o(ft,(function(r){return{o:t(r),aT:n.aT,aU:n.aU}}),Iu))})),Ku=o(zu,"pointerdown",Lu),Pu=o(zu,"pointerout",Lu),Xu=Gr("span"),Wu=Fr,Hu=o(Xu,A([pu("flex items-center gap-2 text-3xl font-bold bg-sky-200 px-2 py-1 rounded")]),A([o(Xu,A([pu("text-red-500")]),A([Wu("B")])),o(Xu,A([pu("text-blue-500")]),A([Wu("i")])),o(Xu,A([pu("text-green-500")]),A([Wu("r")])),o(Xu,A([pu("text-yellow-500")]),A([Wu("t")])),o(Xu,A([pu("text-purple-500")]),A([Wu("h")])),o(Xu,A([pu("text-red-500")]),A([Wu("d")])),o(Xu,A([pu("text-blue-500")]),A([Wu("a")])),o(Xu,A([pu("text-green-500")]),A([Wu("y")])),o(Xu,A([pu("text-yellow-500")]),A([Wu(" ")])),o(Xu,A([pu("text-purple-500")]),A([Wu("E")])),o(Xu,A([pu("text-red-500")]),A([Wu("d")])),o(Xu,A([pu("text-blue-500")]),A([Wu("i")])),o(Xu,A([pu("text-green-500")]),A([Wu("t")])),o(Xu,A([pu("text-yellow-500")]),A([Wu("i")])),o(Xu,A([pu("text-purple-500")]),A([Wu("o")])),o(Xu,A([pu("text-red-500")]),A([Wu("n")]))])),Vu=Gr("td"),Qu=Gr("tr"),Zu=function(r){var n=r.a,t=r.b;return o(Qu,x,A([o(Vu,A([pu("uppercase font-semibold")]),A([Wu(n)])),o(Vu,x,A([Wu(Gn(t))]))]))},ra={$:12},na=hu("alt"),ta=Gr("h1"),ea=o(zu,"pointerdown",{aT:!1,aU:!0}),ua=Gr("p"),aa=function(r){return o(ku,A([yu(A([m("hidden",!r.aL)])),pu("fixed top-0 left-0 bg-white bg-opacity-50 w-screen h-screen flex items-start justify-center p-16"),Ku((function(){return ra}))]),A([o(ku,A([xu,pu("prose bg-white border border-gray-900 rounded max-h-[90%] overflow-auto p-16 shadow-xl relative max-w-[90%]"),f(zu,"pointerdown",{aT:!1,aU:!0},(function(){return Gt}))]),A([o(ta,x,A([Wu("Credits")])),o(ua,A([pu("italic")]),A([Wu("This game is dedicated to my Mom. Happy Birthday!")])),o(ua,x,A([Wu("Game design and development: Tristan")])),o(ua,x,A([Wu("Supplemental art: Yang")])),o(ku,A([pu("bg-sky-200 rounded-xl relative")]),A([o(Bu,A([Tu("pup_transparent.png"),na("Confused Dog"),o(Ur,"width",Gn(400))]),x)])),o(du,A([$u,pu("mt-8"),ea((function(){return ra}))]),A([Wu("Close")]))]))]))},ia=Gr("strong"),oa=function(r){return o(ia,x,A([Wu("["+r+"]")]))},fa=t((function(r,n){var t=oa(Gn(r+1));switch(n.$){case 0:return o(ku,x,x);case 1:return o(du,A([$u,ea((function(){return Jt(r)}))]),A([Wu("Start "),t]));case 2:return o(du,A([$u,ea((function(){return Mt(r)}))]),A([Wu("Serve "),t]));default:return o(du,A([$u,ea((function(){return Ut(r)}))]),A([Wu("Toss "),t]))}})),ca=function(r){switch(r){case 0:return"Rare";case 1:return"Medium";default:return"Well Done"}},sa=C,va=pu("bg-gray-200"),la=Mr,ba=e((function(r,n,t){var e=sa(100*t)+"%",u=function(){switch(n){case 0:return pu("bg-red-200");case 1:return pu("bg-red-700");default:return pu("bg-red-900")}}(),a=Qe(n),i=a.a,f=a.b,c=m(Gn(i)+"%",Gn(f-i)+"%"),s=c.a,v=c.b;return o(ku,A([pu("w-full h-full relative overflow-hidden"),va]),A([o(ku,A([pu("h-full absolute text-gray-100 flex items-center justify-center text-sm"),u,o(la,"left",s),o(la,"width",v)]),A([Wu(ca(n))])),o(Bu,A([pu("absolute z-1 w-[8%]"),o(la,"left",e),o(la,"top","50%"),o(la,"transform","translateY(-50%)"),Tu("Patty3.png")]),x)]))})),da=t((function(r,n){switch(n.$){case 0:return o(ku,A([pu("p-2 italic")]),x);case 1:var t=n.a;return o(ku,A([pu("p-2 w-full h-full flex items-center gap-1 bg-green-100")]),A([o(Xu,x,A([Wu("Order up:")])),o(Xu,A([pu("italic")]),A([Wu(ca(t))]))]));case 2:t=n.a;var e=n.b;return o(ku,A([pu("w-full h-full")]),A([f(ba,r,t,e)]));default:return o(ku,A([pu("p-2 font-bold w-full h-full flex items-center gap-1 bg-red-100")]),A([Wu("Burnt!")]))}})),ga=e((function(r,n,t){var e=n+1;return o(ku,A([pu("flex items-center w-full border border-black overflow-hidden h-16 cursor-pointer"),Ku((function(){return{$:10,a:n}}))]),A([o(ku,A([pu("w-28 border-r border-black h-full flex items-center justify-center")]),A([o(ku,x,A([Wu("Station "+Gn(e))]))])),o(ku,A([pu("w-full h-full border-r border-black flex items-center")]),A([o(da,r,t)])),o(ku,A([pu("w-28 h-full flex items-center justify-center")]),A([o(fa,n,t)]))]))})),ha=S,pa=Gr("table"),$a=e((function(r,n,t){r:for(;;){if(r>0){if(n.b){var e=n.a;r-=1,n=n.b,t=o(mn,e,t);continue r}return t}return t}})),ma=t((function(r,n){return Dn(f($a,r,n,x))})),wa=e((function(r,n,t){if(n>0){var e=m(n,t);r:for(;;){n:for(;;){if(!e.b.b)return t;if(!e.b.b.b){if(1===e.a)break r;break n}switch(e.a){case 1:break r;case 2:var u=e.b;return A([u.a,u.b.a]);case 3:if(e.b.b.b.b){var a=e.b,i=a.b;return A([a.a,i.a,i.b.a])}break n;default:if(e.b.b.b.b&&e.b.b.b.b.b){var c=e.b,s=c.b,v=s.b,l=v.b,b=l.b;return o(mn,c.a,o(mn,s.a,o(mn,v.a,o(mn,l.a,r>1e3?o(ma,n-4,b):f(wa,r+1,n-4,b)))))}break n}}return t}return A([e.b.a])}return x})),ya=t((function(r,n){return f(wa,0,r,n)}));Te={Main:{init:kt({be:function(r){return m({F:r.F,r:St(r.aW),aL:!1,a:At},_t)},bj:function(r){var n,t=r.a;if(1===t.$){var e=t.a,u=o(ft,It(r),o(ft,Ce,o(jt,"key",Le))),a=o(ft,Ct(r),o(ft,Ce,o(jt,"key",Le)));return e.x>0?Lt(A([(n=Nt,ue(Kt(n))),Ne(u),Ee(a)])):zt}return zt},bk:fu,bl:function(r){return o(ku,A([xu,pu("w-full p-10")]),A([o(ku,A([pu("w-full")]),A([o(ku,A([pu("flex items-center justify-between w-1/2")]),A([o(Xu,A([pu("text-3xl font-bold")]),A([Wu("Burger Boss")])),Hu]))])),o(ku,A([pu("flex gap-1 w-full")]),A([function(){var n=pu("w-1/2 relative min-h-96 border border-gray-900 rounded-2xl p-4 transition-all"),t=r.a;switch(t.$){case 1:var e=t.a;return o(ku,A([xu,n,pu("bg-gray-100")]),A([o(ku,A([pu("w-full flex items-center justify-center")]),A([o(ku,A([pu("prose")]),A([Wu("Burgers "),Wu("served: "),o(ia,x,A([Wu(Gn(e.B))]))]))])),o(ku,A([xu,pu("w-full")]),o(qn,ga(e.G),e.d)),o(ku,A([pu("w-full flex gap-12 items-center justify-center h-16")]),A([Su(e),o(du,A([f(zu,"pointerdown",{aT:!0,aU:!1},(function(){return Ft})),f(zu,"pointerup",{aT:!0,aU:!1},(function(){return Ot})),Pu((function(){return Ot})),$u,pu("bg-yellow-500 hover:bg-yellow-400 active:bg-yellow-300"),yu(A([m("bg-yellow-500",!e.G),m("bg-yellow-300",e.G)]))]),A([Wu("Activate Jets! "),oa("Space")])),_u(e)]))]));case 2:return o(ku,A([xu,n]),A([o(ku,A([pu("w-full h-full bg-contain bg-no-repeat bg-center flex items-start justify-end bg-opacity-50"),o(la,"background-image","url('storefront-closed.png')")]),A([o(ku,A([pu("text-red-500 text-6xl font-bold")]),A([Wu("Game Over")]))]))]));default:return o(ku,A([xu,n]),A([o(ku,A([pu("w-full h-full bg-contain bg-no-repeat bg-center"),o(la,"background-image","url('storefront-open.png')")]),x)]))}}(),function(){var n=pu("flex-grow p-4"),t=r.a;switch(t.$){case 1:var e=t.a;return o(ku,A([xu,n]),A([o(ku,A([pu("flex items-center gap-1 text-xl border border-gray-900 px-4 py-2")]),A([o(ku,x,A([Wu("Day timer:")])),o(ku,x,A([Wu(Gn(o(Qn,0,Hn(e.x/1e3))))]))])),o(du,A([$u,pu("bg-red-300 hover:bg-red-200"),Ku((function(){return cu}))]),A([Wu("Close early")])),o(ku,A([xu,pu("w-full border-t border-black mt-24")]),A([o(ku,x,A([Wu("Keyboard Shortcuts")])),o(ku,x,A([oa("1"),o(Xu,x,A([Wu(", ")])),oa("2"),o(Xu,x,A([Wu(", ")])),oa("3"),o(Xu,x,A([Wu(", ")])),oa("4"),o(Xu,x,A([Wu(", ")])),oa("5"),o(Xu,x,A([Wu(": Start, Serve, or Toss burgers")]))])),o(ku,x,A([oa("Space"),o(Xu,x,A([Wu(": Activate Jets!")]))]))]))]));case 0:return o(ku,A([xu,n,pu("gap-8")]),A([o(du,A([$u,pu("bg-green-300 hover:bg-green-200 active:bg-green-100"),Ku((function(){return lu}))]),A([Wu("New Game")])),o(du,A([$u,Ku((function(){return su}))]),A([Wu("Credits")])),aa(r),o(Nu,A([pu("w-full text-gray-900")]),x),o(ku,A([pu("prose prose-sm md:prose-base")]),A([o(Eu,x,A([Wu("Top Burger Bosses")])),o(pa,x,o(ht,Zu,o(ya,10,Dn(o(ha,wu,o(bu,A([m("Baffi",2),m("Bambi",3),m("Lv Bu",10)]),o(ht,(function(r){return m("you",r)}),r.F)))))))]))]));default:var u=t.a;return o(ku,A([xu,n,pu("gap-6")]),A([o(Eu,A([pu("font-semibold text-2xl leading-none")]),A([Wu("Burgers served")])),o(Eu,A([pu("font-semibold text-2xl leading-none")]),A([Wu(Gn(u))])),o(du,A([$u,Ku((function(){return vu}))]),A([Wu("Main Menu")]))]))}}()]))]))}})(o(ot,(function(r){return o(ot,(function(n){return{$:0,a:{F:n,aW:r}}}),o(jt,"highScores",{$:3,b:Et}))}),o(jt,"initialSeed",Et)))(0)}},r.Elm?function r(n,t){for(var e in t)e in n?"init"==e?d(6):r(n[e],t[e]):n[e]=t[e]}(r.Elm,Te):r.Elm=Te}(r);const n=localStorage.getItem("highScores"),t=n?JSON.parse(n):[];r.Elm.Main.init({node:document.querySelector("main"),flags:{initialSeed:Math.floor(999999*Math.random()),highScores:t}}).ports.saveScores.subscribe((function(r){localStorage.setItem("highScores",JSON.stringify(r))}));
//# sourceMappingURL=index.d787e7d8.js.map
