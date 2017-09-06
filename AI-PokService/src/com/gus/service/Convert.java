package com.gus.service;

import java.util.HashMap;
import java.util.Map;

public class Convert {

	public static Map<String, String> convertMap = new HashMap<String, String>();
	static {
		convertMap.put("101", "0x3A");
		convertMap.put("102", "0x2A");
		convertMap.put("103", "0x1A");
		convertMap.put("104", "0x0A");
		convertMap.put("111", "0x3B");
		convertMap.put("112", "0x2B");
		convertMap.put("113", "0x1B");
		convertMap.put("114", "0x0B");
		convertMap.put("121", "0x3C");
		convertMap.put("122", "0x2C");
		convertMap.put("123", "0x1C");
		convertMap.put("124", "0x0C");
		convertMap.put("131", "0x3D");
		convertMap.put("132", "0x2D");
		convertMap.put("133", "0x1D");
		convertMap.put("134", "0x0D");
		convertMap.put("141", "0x31");
		convertMap.put("142", "0x21");
		convertMap.put("143", "0x11");
		convertMap.put("144", "0x01");
		convertMap.put("151", "0x32");
		convertMap.put("152", "0x22");
		convertMap.put("153", "0x12");
		convertMap.put("154", "0x02");
		convertMap.put("31", "0x33");
		convertMap.put("32", "0x23");
		convertMap.put("33", "0x13");
		convertMap.put("34", "0x03");
		convertMap.put("41", "0x34");
		convertMap.put("42", "0x24");
		convertMap.put("43", "0x14");
		convertMap.put("44", "0x04");
		convertMap.put("51", "0x35");
		convertMap.put("52", "0x25");
		convertMap.put("53", "0x15");
		convertMap.put("54", "0x05");
		convertMap.put("61", "0x36");
		convertMap.put("62", "0x26");
		convertMap.put("63", "0x16");
		convertMap.put("64", "0x06");
		convertMap.put("71", "0x37");
		convertMap.put("72", "0x27");
		convertMap.put("73", "0x17");
		convertMap.put("74", "0x07");
		convertMap.put("81", "0x38");
		convertMap.put("82", "0x28");
		convertMap.put("83", "0x18");
		convertMap.put("84", "0x08");
		convertMap.put("91", "0x39");
		convertMap.put("92", "0x29");
		convertMap.put("93", "0x19");
		convertMap.put("94", "0x09");
		convertMap.put("99", "0x4E");
		convertMap.put("100", "0x4F");
	}

	/**
	 * 接口交互牌面值： #
	 * 方块 A-K
	 * 0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D 
	 * #梅花 A-K
	 * 0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D 
	 * #红桃 A-K
	 * 0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D 
	 * #黑桃 A-K
	 * 0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D 
	 * #小王 大王
	 * 0x4E,0x4F
	 * 
	 * 内部使用牌面值 #方块A-K 24, 34, 44, 54, 64, 74, 84,94, 104, 114, 124, 134，144
	 * #梅花A-K 23, 33, 43, 53, 63, 73, 83,93, 103, 113, 123, 133，143 #红桃 A-K 22,
	 * 32, 42, 52, 62, 72, 82,92, 102, 112, 122, 132，142 #黑桃 A-K 21, 31, 41, 51,
	 * 61, 71, 81,91, 101, 111, 121, 131，141 #小王 大王 99,100
	 */
	public static String convert(String rep) {

		String s = rep.replace(",101", ",0x3A").replace(",102", ",0x2A").replace(",103", ",0x1A")
				.replace(",104", ",0x0A").replace(",111", ",0x3B").replace(",112", ",0x2B").replace(",113", ",0x1B")
				.replace(",114", ",0x0B").replace(",121", ",0x3C").replace(",122", ",0x2C").replace(",123", ",0x1C")
				.replace(",124", ",0x0C").replace(",131", ",0x3D").replace(",132", ",0x2D").replace(",133", ",0x1D")
				.replace(",134", ",0x0D").replace(",141", ",0x31").replace(",142", ",0x21").replace(",143", ",0x11")
				.replace(",144", ",0x01").replace(",151", ",0x32").replace(",152", ",0x22").replace(",153", ",0x12")
				.replace(",154", ",0x02").replace(",31", ",0x33").replace(",32", ",0x23").replace(",33", ",0x13")
				.replace(",34", ",0x03").replace(",41", ",0x34").replace(",42", ",0x24").replace(",43", ",0x14")
				.replace(",44", ",0x04").replace(",51", ",0x35").replace(",52", ",0x25").replace(",53", ",0x15")
				.replace(",54", ",0x05").replace(",61", ",0x36").replace(",62", ",0x26").replace(",63", ",0x16")
				.replace(",64", ",0x06").replace(",71", ",0x37").replace(",72", ",0x27").replace(",73", ",0x17")
				.replace(",74", ",0x07").replace(",81", ",0x38").replace(",82", ",0x28").replace(",83", ",0x18")
				.replace(",84", ",0x08").replace(",91", ",0x39").replace(",92", ",0x29").replace(",93", ",0x19")
				.replace(",94", ",0x09").replace(",99", ",0x4E").replace(",100", ",0x4F");


		String s1 = s.split(",")[0];

		String f = convertMap.get(s1);

		return s.replaceFirst(s1, f);
		

		// .replaceFirst("101", "0x3A").replaceFirst("102",
		// "0x2A").replaceFirst("103", "0x1A").replaceFirst("104", "0x0A")
		// .replaceFirst("111", "0x3B").replaceFirst("112",
		// "0x2B").replaceFirst("113", "0x1B").replaceFirst("114", "0x0B")
		// .replaceFirst("121", "0x3C").replaceFirst("122",
		// "0x2C").replaceFirst("123", "0x1C").replaceFirst("124", "0x0C")
		// .replaceFirst("131", "0x3D").replaceFirst("132",
		// "0x2D").replaceFirst("133", "0x1D").replaceFirst("134", "0x0D")
		// .replaceFirst("141", "0x31").replaceFirst("142",
		// "0x21").replaceFirst("143", "0x11").replaceFirst("144", "0x01")
		// .replaceFirst("151", "0x32").replaceFirst("152",
		// "0x22").replaceFirst("153", "0x12").replaceFirst("154", "0x02")
		// .replaceFirst("31", "0x33").replaceFirst("32",
		// "0x23").replaceFirst("33", "0x13").replaceFirst("34", "0x03")
		// .replaceFirst("41", "0x34").replaceFirst("42",
		// "0x24").replaceFirst("43", "0x14").replaceFirst("44", "0x04")
		// .replaceFirst("51", "0x35").replaceFirst("52",
		// "0x25").replaceFirst("53", "0x15").replaceFirst("54", "0x05")
		// .replaceFirst("61", "0x36").replaceFirst("62",
		// "0x26").replaceFirst("63", "0x16").replaceFirst("64", "0x06")
		// .replaceFirst("71", "0x37").replaceFirst("72",
		// "0x27").replaceFirst("73", "0x17").replaceFirst("74", "0x07")
		// .replaceFirst("81", "0x38").replaceFirst("82",
		// "0x28").replaceFirst("83", "0x18").replaceFirst("84", "0x08")
		// .replaceFirst("91", "0x39").replaceFirst("92",
		// "0x29").replaceFirst("93", "0x19").replaceFirst("94", "0x09")
		// .replaceFirst("99", "0x4E").replaceFirst("100", "0x4F");

		// .replace("\"101", "\"0x3A").replace("\"102",
		// "\"0x2A").replace("\"103", "\"0x1A").replace("\"104", "\"0x0A")
		// .replace("\"111", "\"0x3B").replace("\"112",
		// "\"0x2B").replace("\"113", "\"0x1B").replace("\"114", "\"0x0B")
		// .replace("\"121", "\"0x3C").replace("\"122",
		// "\"0x2C").replace("\"123", "\"0x1C").replace("\"124", "\"0x0C")
		// .replace("\"131", "\"0x3D").replace("\"132",
		// "\"0x2D").replace("\"133", "\"0x1D").replace("\"134", "\"0x0D")
		// .replace("\"141", "\"0x31").replace("\"142",
		// "\"0x21").replace("\"143", "\"0x11").replace("\"144", "\"0x01")
		// .replace("\"151", "\"0x32").replace("\"152",
		// "\"0x22").replace("\"153", "\"0x12").replace("\"154", "\"0x02")
		// .replace("\"31", "\"0x33").replace("\"32", "\"0x23").replace("\"33",
		// "\"0x13").replace("\"34", "\"0x03")
		// .replace("\"41", "\"0x34").replace("\"42", "\"0x24").replace("\"43",
		// "\"0x14").replace("\"44", "\"0x04")
		// .replace("\"51", "\"0x35").replace("\"52", "\"0x25").replace("\"53",
		// "\"0x15").replace("\"54", "\"0x05")
		// .replace("\"61", "\"0x36").replace("\"62", "\"0x26").replace("\"63",
		// "\"0x16").replace("\"64", "\"0x06")
		// .replace("\"71", "\"0x37").replace("\"72", "\"0x27").replace("\"73",
		// "\"0x17").replace("\"74", "\"0x07")
		// .replace("\"81", "\"0x38").replace("\"82", "\"0x28").replace("\"83",
		// "\"0x18").replace("\"84", "\"0x08")
		// .replace("\"91", "\"0x39").replace("\"92", "\"0x29").replace("\"93",
		// "\"0x19").replace("\"94", "\"0x09")
		// .replace("\"99", "\"0x4E").replace("\"100", "\"0x4F");

	}

	public static void main(String[] args) {

		System.out.println(convert("33,144,143,142,141,131,121,111,101,100,92,91,81,71,53,52,31"));

	}
}
