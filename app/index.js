import Elm from './Main';

console.info('elm', Elm, Elm.Main);

Elm.embed(Elm.Main, document.getElementById('main'));
