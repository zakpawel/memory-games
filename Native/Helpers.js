Elm.Native.Helpers = {}
Elm.Native.Helpers.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {}
	localRuntime.Native.Helpers = localRuntime.Native.Helpers || {}
	if (localRuntime.Native.Helpers.values) {
		return localRuntime.Native.Helpers.values;
	}

	var Maybe = Elm.Maybe.make(localRuntime);
	var Task = Elm.Native.Task.make(localRuntime);

	function storageFail() {
		return Task.fail({ ctor: "NoLocalStorage" });
	}

	function setFn(key, val) {
		localStorage.setItem(key, val);
		return Task.succeed(Maybe.Just(val));
	}

	function getFn(key) {
		var val = localStorage.getItem(key);
		return Task.succeed(val === null ? Maybe.Nothing : Maybe.Just(val));
	}

	function randomFloat() {
		return Task.succeed(Math.random());
	}

	function print(s) {
		console.log(s);
		return Task.succeed({
			ctor: "_Tuple0"
		});
	}

	if (!localStorage) {
		return localRuntime.Native.Helpers.values = {
			get: storageFail(),
			set: storageFail()
		}
	}

	localRuntime.Native.Helpers.values = {
		set: F2(setFn),
		get: getFn,
		print: print,
		randomFloat: randomFloat
	}

	return localRuntime.Native.Helpers.values;
};