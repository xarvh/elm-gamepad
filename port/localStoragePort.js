function addLocalStoragePorts(elmApp) {

  /*
  elmApp.ports.localStorageRequest.subscribe(function (key) {
    elmApp.ports.localStorageReceive.send([key, localStorage[key]]);
  })
  */

  elmApp.ports.localStorageSet.subscribe(function (args) {
    localStorage.setItem(args.key, args.value);
  });
}
