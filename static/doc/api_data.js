define({ "api": [
  {
    "type": "get",
    "url": "{{base_url}}/lobby/create",
    "title": "create Lobby",
    "name": "create",
    "group": "Lobby",
    "header": {
      "fields": {
        "Header": [
          {
            "group": "Header",
            "type": "String",
            "optional": false,
            "field": "auth",
            "description": "<p>Users auth Token.</p>"
          }
        ]
      }
    },
    "description": "<p>create a new Lobby</p>",
    "version": "0.0.0",
    "filename": "src/Handler/Lobby.hs",
    "groupTitle": "Lobby"
  },
  {
    "type": "get",
    "url": "{{base_url}}/lobby/find",
    "title": "find Lobbys",
    "name": "find",
    "group": "Lobby",
    "description": "<p>find all public lobbys of the last hour</p>",
    "header": {
      "fields": {
        "Header": [
          {
            "group": "Header",
            "type": "String",
            "optional": false,
            "field": "auth",
            "description": "<p>Users auth Token.</p>"
          }
        ]
      }
    },
    "version": "0.0.0",
    "filename": "src/Handler/Lobby.hs",
    "groupTitle": "Lobby"
  },
  {
    "type": "post",
    "url": "{{base_url}}/lobby/join/:salt",
    "title": "join Lobby",
    "name": "join",
    "group": "Lobby",
    "parameter": {
      "fields": {
        "Parameter": [
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "salt",
            "description": "<p>Unique salt of the lobby</p>"
          }
        ]
      }
    },
    "header": {
      "fields": {
        "Header": [
          {
            "group": "Header",
            "type": "String",
            "optional": false,
            "field": "auth",
            "description": "<p>Users auth Token.</p>"
          }
        ]
      }
    },
    "description": "<p>join a lobby by a given salt</p>",
    "version": "0.0.0",
    "filename": "src/Handler/Lobby.hs",
    "groupTitle": "Lobby"
  },
  {
    "type": "post",
    "url": "{{base_url}}/lobby/:lobbyId/kick/:userId",
    "title": "kick from Lobby",
    "name": "kick",
    "group": "Lobby",
    "parameter": {
      "fields": {
        "Parameter": [
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "lobbyId",
            "description": "<p>UUID of the lobby you are host</p>"
          },
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "userId",
            "description": "<p>UUID of the user you want to kick</p>"
          }
        ]
      }
    },
    "header": {
      "fields": {
        "Header": [
          {
            "group": "Header",
            "type": "String",
            "optional": false,
            "field": "auth",
            "description": "<p>Users auth Token</p>"
          }
        ]
      }
    },
    "description": "<p>kick a player from the Lobby (host is allowed only)</p>",
    "version": "0.0.0",
    "filename": "src/Handler/Lobby.hs",
    "groupTitle": "Lobby"
  },
  {
    "type": "post",
    "url": "{{base_url}}/lobby/:lobbyId/launch",
    "title": "launch Gamee",
    "name": "launch",
    "group": "Lobby",
    "parameter": {
      "fields": {
        "Parameter": [
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "lobbyId",
            "description": "<p>Id of the current Lobby</p>"
          }
        ]
      }
    },
    "header": {
      "fields": {
        "Header": [
          {
            "group": "Header",
            "type": "String",
            "optional": false,
            "field": "auth",
            "description": "<p>Users auth Token.</p>"
          }
        ]
      }
    },
    "description": "<p>launch a game (players must be 4&lt;=p&lt;=6)</p>",
    "version": "0.0.0",
    "filename": "src/Handler/Lobby.hs",
    "groupTitle": "Lobby"
  },
  {
    "type": "post",
    "url": "{{base_url}}/lobby/:lobbyId/leave",
    "title": "leave Lobby",
    "name": "leave",
    "group": "Lobby",
    "parameter": {
      "fields": {
        "Parameter": [
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "lobbyId",
            "description": "<p>UUID of the Lobby</p>"
          }
        ]
      }
    },
    "header": {
      "fields": {
        "Header": [
          {
            "group": "Header",
            "type": "String",
            "optional": false,
            "field": "auth",
            "description": "<p>Users auth Token</p>"
          }
        ]
      }
    },
    "description": "<p>leave a lobby you have joined previously</p>",
    "version": "0.0.0",
    "filename": "src/Handler/Lobby.hs",
    "groupTitle": "Lobby"
  },
  {
    "type": "post",
    "url": "{{base_url}}/lobby/:lobbyId/status",
    "title": "Lobby status",
    "name": "status",
    "group": "Lobby",
    "parameter": {
      "fields": {
        "Parameter": [
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "lobbyId",
            "description": "<p>Id of the current Lobby</p>"
          }
        ]
      }
    },
    "header": {
      "fields": {
        "Header": [
          {
            "group": "Header",
            "type": "String",
            "optional": false,
            "field": "auth",
            "description": "<p>Users auth Token.</p>"
          }
        ]
      }
    },
    "description": "<p>get the status of the current lobby (in case you are a part of it)</p>",
    "version": "0.0.0",
    "filename": "src/Handler/Lobby.hs",
    "groupTitle": "Lobby"
  }
] });
