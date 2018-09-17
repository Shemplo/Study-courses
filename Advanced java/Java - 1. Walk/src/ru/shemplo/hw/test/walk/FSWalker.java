package ru.shemplo.hw.test.walk;

import java.io.File;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class FSWalker {

	private static Predicate <String> testExtension, testDirectory;
	private static Consumer <File> actionFile, actionFolder;

	public synchronized static void crawlFS (File file, Predicate <String> dirTester, 
												Predicate <String> extTester, 
												Consumer <File> actionFolder,
												Consumer <File> actionFile) {
		FSWalker.actionFolder = actionFolder;
		FSWalker.testDirectory = dirTester;
		FSWalker.testExtension = extTester;
		FSWalker.actionFile = actionFile;
		_crawler (file, "");
	}

	private static void _crawler (File file, String prefix) {
		if (file.exists () && file.isDirectory ()) {
			String [] files = file.list ();
			if (files == null || !testDirectory.test (prefix)) { return; }

			for (String fileName : files) {
				File levelFile = new File (file, fileName);
				_crawler (levelFile, prefix + "/" + fileName);
			}
			
			actionFolder.accept (file);
		} else if (file.exists () && file.isFile ()) {
			String fileName = file.getName ();
			String extension = getFileExtension (fileName);
			if (!testExtension.test (extension)) { return; }

			actionFile.accept (file);
		}
	}

	private static String getFileExtension (String fileName) {
		if (fileName == null) { return ""; }

		int index = fileName.lastIndexOf ('.');
		if (index != -1) { return fileName.substring (index); }

		return "";
	}

}
