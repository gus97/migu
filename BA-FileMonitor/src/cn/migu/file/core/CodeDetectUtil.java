package cn.migu.file.core;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.security.MessageDigest;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.codec.digest.DigestUtils;

import info.monitorenter.cpdetector.io.ASCIIDetector;
import info.monitorenter.cpdetector.io.CodepageDetectorProxy;
import info.monitorenter.cpdetector.io.JChardetFacade;
import info.monitorenter.cpdetector.io.ParsingDetector;
import info.monitorenter.cpdetector.io.UnicodeDetector;

public class CodeDetectUtil {
	/**
	 * 判断文件的编码格式
	 * 
	 * @param fileName
	 *            :file
	 * @return 文件编码格式
	 * @throws IOException
	 * @throws Exception
	 */
	public static String codeString(String fileName) throws IOException
	{
		BufferedInputStream bin = new BufferedInputStream(new FileInputStream(fileName));
		int p = (bin.read() << 8) + bin.read();
		String code = null;
		// 其中的 0xefbb、0xfffe、0xfeff、0x5c75这些都是这个文件的前面两个字节的16进制数

		switch (p) {
		case 0xefbb:
			code = "UTF-8";
			break;
		case 0xfffe:
			code = "Unicode";
			break;
		case 0xfeff:
			code = "UTF-16BE";
			break;
		case 0x5c75:
			code = "ANSI|ASCII";
			break;
		default:
			code = "GBK";
		}
		bin.close();
		return code;
	}

	public static int fileLength(String fileName) {
		File fn = new File(fileName);
		long fileLength = fn.length();
		try (LineNumberReader rf = new LineNumberReader(new FileReader(fn))) {
			if (rf != null) {
				int lines = 0;
				rf.skip(fileLength);
				lines = rf.getLineNumber();
				return lines;
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return 0;
	}

	public static String getDigestOfFile(String path) {
		MessageDigest messagedigest;
		FileInputStream in = null;
		try {
			in = new FileInputStream(new File(path));
			messagedigest = MessageDigest.getInstance("SHA-1");
			byte[] buffer = new byte[1024 * 1024 * 10];
			int len = 0;

			while ((len = in.read(buffer)) > 0) {
				// 该对象通过使用 update（）方法处理数据
				messagedigest.update(buffer, 0, len);
			}

			return DigestUtils.sha1Hex(messagedigest.digest());
		} catch (Exception e) {
			e.printStackTrace();
			return "error";
		} finally {
			if (null != in) {
				try {
					in.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}

	public static String getFileEncode(String path) {
		/*
		 * detector是探测器，它把探测任务交给具体的探测实现类的实例完成。
		 * cpDetector内置了一些常用的探测实现类，这些探测实现类的实例可以通过add方法 加进来，如ParsingDetector、
		 * JChardetFacade、ASCIIDetector、UnicodeDetector。
		 * detector按照“谁最先返回非空的探测结果，就以该结果为准”的原则返回探测到的
		 * 字符集编码。使用需要用到三个第三方JAR包：antlr.jar、chardet.jar和cpdetector.jar
		 * cpDetector是基于统计学原理的，不保证完全正确。
		 */
		CodepageDetectorProxy detector = CodepageDetectorProxy.getInstance();
		/*
		 * ParsingDetector可用于检查HTML、XML等文件或字符流的编码,构造方法中的参数用于
		 * 指示是否显示探测过程的详细信息，为false不显示。
		 */
		detector.add(new ParsingDetector(false));
		/*
		 * JChardetFacade封装了由Mozilla组织提供的JChardet，它可以完成大多数文件的编码
		 * 测定。所以，一般有了这个探测器就可满足大多数项目的要求，如果你还不放心，可以
		 * 再多加几个探测器，比如下面的ASCIIDetector、UnicodeDetector等。
		 */
		detector.add(JChardetFacade.getInstance());// 用到antlr.jar、chardet.jar
		// ASCIIDetector用于ASCII编码测定
		detector.add(ASCIIDetector.getInstance());
		// UnicodeDetector用于Unicode家族编码的测定
		detector.add(UnicodeDetector.getInstance());
		java.nio.charset.Charset charset = null;
		File f = new File(path);
		try {
			charset = detector.detectCodepage(f.toURI().toURL());
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		if (charset != null)
			return charset.name();
		else
			return null;
	}

	public static Set<String> getFileList(String strPath) {
		Set<String> filelist = new HashSet<String>();
		File dir = new File(strPath);
		File[] files = dir.listFiles(); // 该文件目录下文件全部放入数组
		if (files != null) {
			for (int i = 0; i < files.length; i++) {
				if (!files[i].isDirectory()) { // 判断是文件还是文件夹
					filelist.add(files[i].toString());
				}
			}
		}
		return filelist;
	}

	//|^AMGP_\\d+\\.\\d+$
	public static void main(String[] args) {
		Pattern pattern = Pattern.compile("^CMGP_\\d+\\.\\d+$|^AMGP_\\d+\\.\\d+$");
		Matcher matcher = pattern.matcher("AMGP_1.1");
		boolean b = matcher.matches();
		System.out.println(b);
		
//		String encode = EncodingDetect.getJavaEncode("c:/1.txt");
//		System.out.println(encode);
	}

}
