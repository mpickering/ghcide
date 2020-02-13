// Copyright (c) 2019 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

import * as path from 'path';
import { workspace, ExtensionContext, window } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	let config = workspace.getConfiguration("hic");
	let cPath: string = config.get("executablePath") as string;
	if(cPath === "" || cPath === undefined){
		window.showErrorMessage("You must specify a hic.executionPath in config");
		return;
	}

	let argString = config.get("arguments") as string;

	// doesn't work if you split - you get things like 'Invalid option `--lsp''
	//let args : string[] = argString.split(" ");

	let workingDirectory = config.get("workingDirectory") as string;

	let serverOptions = {
		args: [argString],
		command: cPath,
		options: {
			cwd: workingDirectory !== undefined ? workingDirectory : workspace.rootPath
		}
	};

	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: ["haskell"]
	};

	client = new LanguageClient('haskell', 'ghcide', serverOptions, clientOptions, true);
    client.registerProposedFeatures();

	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
