package cn.migu.file.core;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;

import org.apache.commons.io.FileUtils;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

public class FileWriteUtil {

	// private static SimpleDateFormat format = new SimpleDateFormat("yyyy/MM/dd
	// HH:mm:ss");

	private static XSSFWorkbook hssfWorkbook;

	public static void Write(String path, int count) throws IOException, InterruptedException {

		BufferedOutputStream buff = new BufferedOutputStream(new FileOutputStream(path));

		for (int i = 0; i < count; i++) {

			// buff.write(
			// (format.format(new Date()) + " INFO " + i + " " +
			// getRandomString(16) + " " + getIntervalNum(10, 20)
			// + " " + getRandomString(32) + " " + getIntervalNum(1, 50000) + "
			// " + getRandomString(16) + "\n")
			// .getBytes());

			// Thread.sleep(1000);
			buff.write(
					(i + "\037" + getRandomString(16) + "\037" + getIntervalNum(10, 20) + "\037" + getRandomString(32)
							+ "\037" + getIntervalNum(1, 50000) + "\037" + getRandomString(16) + "\n").getBytes());

			buff.flush();
		}

		buff.close();
	}

	public static void main(String[] args) throws IOException {
		// Write("/10m.a", 1000000);
		// readXls2Txt("c:/1/1.xlsx","c:/1/33.log","\037");
		// getXlsandx("F:/zdy/s1/2.xlsx", "F:/zdy/r1/2.xlsx-123", "\037",5);

		getXlsandx("c:/1/1.xlsx", "c:/1/22.log", "\037", 4, 1);
	}

	public static String getLongDateString(Long time) {

		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		if (time == null) {
			return sdf.format(new Date());
		}

		Date date = new Date(time);
		return sdf.format(date);
	}

	public static String getRandomString(int len) {
		String seed = "abcdefghijklmnopqrstuvwxyz0123456789";
		Random random = new Random();
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < len; i++) {
			int number = random.nextInt(seed.length());
			sb.append(seed.charAt(number));
		}
		return sb.toString();
	}

	public static String noRepeatNum(int count) {
		if (count > 10) {
			count = 10;
		}
		StringBuffer sb = new StringBuffer();
		String str = "0123456789";
		Random r = new Random();
		for (int i = 0; i < count; i++) {
			int num = r.nextInt(str.length());
			sb.append(str.charAt(num));
			str = str.replace((str.charAt(num) + ""), "");
		}
		return sb.toString();
	}

	public static int getIntervalNum(int min, int max) {

		Random random = new Random();

		int num = random.nextInt(max) % (max - min + 1) + min;

		return num;
	}

	public static void readXls2Txt(String filePath, String cpath, String decollator) throws IOException {

		InputStream is = new FileInputStream(filePath);
		hssfWorkbook = new XSSFWorkbook(is);
		List<String> txtTmp = new ArrayList<String>();

		// 循环工作表Sheet
		for (int numSheet = 0; numSheet < hssfWorkbook.getNumberOfSheets(); numSheet++) {

			XSSFSheet hssfSheet = hssfWorkbook.getSheetAt(numSheet);

			int i = 1;
			if (hssfSheet == null) {
				continue;
			}

			// 循环行Row
			for (int rowNum = 0; rowNum <= hssfSheet.getLastRowNum(); rowNum++) {
				XSSFRow hssfRow = hssfSheet.getRow(rowNum);
				String sb = "";
				if (hssfRow == null) {
					i++;
					continue;
				}

				// 循环列Cell
				for (int cellNum = 0; cellNum <= hssfRow.getLastCellNum(); cellNum++) {
					XSSFCell hssfCell = hssfRow.getCell(cellNum);
					if (hssfCell == null) {
						continue;
					}

					sb += getValue(hssfCell) + decollator;
				}

				sb = sb.substring(0, sb.length() - 1);

				System.out.println(sb + "---------------------------------------------------->>>>>");

				txtTmp.add("  " + hssfSheet.getSheetName() + "-" + (i++) + "," + SqlConstant.SERVER_IP + "," + filePath
						+ "FFFFFa4b6f1e41849c064eef9d0d2a193cd00FFFFF" + sb);
			}
			FileUtils.writeLines(new File(cpath), "UTF-8", txtTmp, true);
			txtTmp.clear();
		}
	}

	@SuppressWarnings({ "deprecation", "static-access" })
	public static String getValue(XSSFCell hssfCell) {
		if (hssfCell.getCellType() == hssfCell.CELL_TYPE_BOOLEAN) {
			return String.valueOf(hssfCell.getBooleanCellValue());
		} else if (hssfCell.getCellType() == CellType.NUMERIC.getCode()) {
			return String.valueOf(hssfCell.getNumericCellValue());
		} else {
			return String.valueOf(hssfCell.getStringCellValue());
		}
	}

	@SuppressWarnings("deprecation")
	public static void getXlsandx(String filePath, String cpath, String decollator, int colNum, int start_line) {
		// ArrayList<Map<String,Object>> xlsMapList = new
		// ArrayList<Map<String,Object>>();

		System.out.println(filePath + "---" + cpath + "---" + decollator + "---" + colNum + "---" + start_line);

		List<String> txtTmp = new ArrayList<String>();
		File xlsOrxlsxFile = new File(filePath);
		if (!xlsOrxlsxFile.exists()) {
			return;
		}
		try {
			Workbook wb = WorkbookFactory.create(xlsOrxlsxFile);
			int sheetNum = wb.getNumberOfSheets();
			Sheet sheet = null;

			for (int sheetIndex = 0; sheetIndex < sheetNum; sheetIndex++) {// 遍历sheet(index
																			// 0开始)
				int i = 1;
				String shn = wb.getSheetAt(sheetIndex).getSheetName();
				// System.out.println("sheet:"+sheetIndex);
				sheet = wb.getSheetAt(sheetIndex);
				Row row = null;
				// 业务变动，指定头
				// int firstRowNum = sheet.getFirstRowNum();
				int firstRowNum = sheet.getFirstRowNum() + start_line;
				int lastRowNum = sheet.getLastRowNum();
				for (int rowIndex = firstRowNum; rowIndex <= lastRowNum; rowIndex++) {// 遍历row(行
																						// 0开始)
					row = sheet.getRow(rowIndex);
					String s = shn + "-" + (i++) + "," + SqlConstant.SERVER_IP + "," + filePath
							+ "FFFFFa4b6f1e41849c064eef9d0d2a193cd00FFFFF";
					if (null != row) {
						int firstCellNum = 0;
						int lastCellNum = colNum;
						for (int cellIndex = firstCellNum; cellIndex < lastCellNum; cellIndex++) {// 遍历cell（列
																									// 0开始）
							Cell cell = row.getCell(cellIndex);

							// System.out.println(cell.toString()+"---");

							if (null != cell) {
								Object cellValue = null;// cellValue的值
								switch (cell.getCellType()) {
								case Cell.CELL_TYPE_STRING:
									cellValue = cell.getRichStringCellValue().getString();
									s += cellValue + decollator;
									break;
								case Cell.CELL_TYPE_NUMERIC:
									if (DateUtil.isCellDateFormatted(cell)) {
										cellValue = cell.getDateCellValue();
										s += cellValue + decollator;
									} else {
										cellValue = cell.getNumericCellValue();
										s += cellValue + decollator;
									}
									break;
								case Cell.CELL_TYPE_BOOLEAN:
									cellValue = cell.getBooleanCellValue();
									s += cellValue + decollator;
									break;
								case Cell.CELL_TYPE_FORMULA:
									cellValue = cell.getCellFormula();
									s += cellValue + decollator;
									break;
								default:

								}
								// System.out.println("value:"+cellValue);
							} else {
								s += decollator;
							}
						} // end cell
						s = s.substring(0, s.length() - 1);
						txtTmp.add(s);
					}
				} // end row

				FileUtils.writeLines(new File(cpath), "UTF-8", txtTmp, true);
				txtTmp.clear();
				//start_line = 0;
			} // end sheet
		} catch (InvalidFormatException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
