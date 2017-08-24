package cn.migu.file.core;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;

import au.com.bytecode.opencsv.CSVReader;

/**
 */
public class CsvParser {
	
	public static void main(String[] args) throws IOException {
		parserCSV2Log("c:/1/555.csv","c:/1/10.log","\037",5);
	}

	public static void parserCSV2Log(String filePath, String cpath, String decollator, int colNum) throws IOException {
		
		List<String> txtTmp = new ArrayList<String>();
		CSVReader csvReader = new CSVReader(new InputStreamReader(new FileInputStream(new File(filePath)), "GBK"), ',');
		String[] strs = csvReader.readNext();
		String s1 = "";
		if (strs != null && strs.length > 0) {
			for (String str : strs) {
				s1 += str + decollator;
			}
			s1 = s1.substring(0, s1.length() - 1);
			txtTmp.add(s1);
		}
		List<String[]> list = csvReader.readAll();
		
		for (String[] ss : list) {
			s1 = "";
			for (String s : ss){
				if(null != s && !s.equals("")) {
					s1 += s+decollator;
				}else{
					s1 +=decollator;
				}
			}
			if(s1.length()>0){
				s1 = s1.substring(0, s1.length() - 1);
				txtTmp.add(s1);
			}
		}
		FileUtils.writeLines(new File(cpath), "UTF-8", txtTmp);
		csvReader.close();
	}

}