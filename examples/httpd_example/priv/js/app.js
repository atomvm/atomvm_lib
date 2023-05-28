//
//     # ----------------------------------------------------------------------------
//     # "THE BEER-WARE LICENSE" (Revision 42):
//     # <fred@dushin.net> wrote this file.  You are hereby granted permission to
//     # copy, modify, or mutilate this file without restriction.  If you create a
//     # work derived from this file, you may optionally include a copy of this notice,
//     # for which I would be most grateful, but you are not required to do so.
//     # If we meet some day, and you think this stuff is worth it, you can buy me a
//     # beer in return.   Fred Dushin
//     # ----------------------------------------------------------------------------
//

var SystemModel = Backbone.Model.extend({
	url: '/api/system_info',
	defaults: {
		platform: null,
		system_architecture: null,
		word_size: null,
		atomvm_version: null,
		esp32_chip_info: null,
		esp_idf_version: null,
		chip_model: null,
		chip_cores: null,
		chip_features: null,
		chip_revision: null
	},

	parse: function(data) {
		data["chip_model"] = data.esp32_chip_info["model"];
		data["chip_cores"] = data.esp32_chip_info["cores"];
		data["chip_features"] = data.esp32_chip_info["features"];
		data["chip_revision"] = data.esp32_chip_info["revision"];
		return data;
	}
});
var system_model = new SystemModel();

var SystemView = Backbone.View.extend({
	el: '#system-view',
	template: _.template($('#system-tmpl').html()),

	initialize: function() {
		this.listenTo(this.model, 'sync change', this.render);
		this.model.fetch();
		this.render();
	},

	render: function() {
		var html = this.template(this.model.toJSON());
		this.$el.html(html);
		return this;
	}
});
var system_view = new SystemView({model: system_model})

var MemoryModel = Backbone.Model.extend({
	url: '/api/memory',
	defaults: {
		atom_count: null,
		port_count: null,
		process_count: null,
		esp32_free_heap_size: null,
		esp32_largest_free_block: null,
		esp32_minimum_free_size: null
	},

	parse: function(data) {
		return data;
	}
});
var memory_model = new MemoryModel();

var MemoryView = Backbone.View.extend({
	el: '#memory-view',
	template: _.template($('#memory-tmpl').html()),

	initialize: function() {
		this.listenTo(this.model, 'sync change', this.render);
		this.model.fetch();
		this.render();
	},

	render: function() {
		var html = this.template(this.model.toJSON());
		this.$el.html(html);
		return this;
	}
});
var memory_view = new MemoryView({model: memory_model})

var get_websocket_url = function() {
	var hostname = window.location.hostname;
	var port = window.location.port;
	return "ws://" + hostname + ":" + port + "/ws";
}

var create_websocket = function() {
	var ws = new WebSocket(get_websocket_url(), []);
	ws.onmessage = function (event) {
		console.log("Received WS data: " + event.data);
		var data = JSON.parse(event.data);
		for (var key in memory_model.attributes) {
			if (data[key]) {
				memory_model.set(key, data[key]);
			}
		}
	}

	ws.onopen = function (event) {
		console.log("opened connection: " + event);
	}

	ws.onclose = function (event) {
		console.log("closed connection: " + event);
		webSocket = create_websocket();
	}
	return ws;
}

var webSocket = create_websocket();
