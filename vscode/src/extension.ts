'use strict';
import { workspace, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient';

let client: LanguageClient; 

export function activate(context: ExtensionContext) {
	console.log("Activating STL Language Support...");

	const config = workspace.getConfiguration("vscode-stl-language-support");

	if (config.executable != '') {
		console.log("Loading STL Language Server...");
		let serverPath = config.executable;
		let serverOptions: ServerOptions = {
			run: { command: serverPath, args: [], transport: TransportKind.stdio },
			debug: { command: serverPath, args: [], transport: TransportKind.stdio }
		};
		let clientOptions: LanguageClientOptions = {
			documentSelector: ['stl']
		};
		let client = new LanguageClient('stl', 'STL Language Server', serverOptions, clientOptions);
		client.start();
	}
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}