Reactive Programming
====================

It seems to be a hot new paradigm for front end javascript development.
Everything is a "stream" of data and you call functions like map, filter,
etc... to transform those streams into things that are useful for you. These
"streams" are called observables in RxJs. This was a fantastic read on the
subject: https://gist.github.com/staltz/868e7e9bc2a7b8c1f754

import { Observable } from 'rxjs/Rx';
  return Observable.interval(1000).switchMap(() => this.http.get(endpoint + '/flags'))
        .map(res => res.json());
