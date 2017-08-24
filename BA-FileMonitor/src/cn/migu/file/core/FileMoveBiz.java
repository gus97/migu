package cn.migu.file.core;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.UUID;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.alibaba.fastjson.JSONObject;


public class FileMoveBiz {

	static private Logger logger = LoggerFactory.getLogger(FileMoveBiz.class);

	static private Map<Pattern, Long> p = new HashMap<Pattern, Long>();

	// 并行队列操作
	public static void moveFile2CollectPath() {
		logger.info("----------------------->\tfile copy quene start ok !");
		for (final Entry<Pattern, BlockingQueue<BlockingQueneFileInfo>> entry : BlockingQueue4Files.bqf.entrySet()) {

			p.put(entry.getKey(), 0L);

			new Thread(new Runnable() {

				@SuppressWarnings({ "static-access", "deprecation" })
				public void run() {

					BlockingQueneFileInfo bf;
					try {
						while (true) {
							// 阻塞，没有文件一直阻塞
							bf = entry.getValue().poll(5L, TimeUnit.MINUTES);
							if (bf == null) {
								p.put(entry.getKey(), p.get(entry.getKey()) + 5L);
								logger.info("----------------------->\tEXP  [" + entry.getKey().toString() + "] until "
										+ p.get(entry.getKey()) + " minute no file!");
								continue;
							}
							;

							System.out
									.println(bf.getDecollator() + "------" + bf.getKind() + "-------" + bf.getColNum());

							// 如果是增量copy，直到文件大小没有发生变化,并且文件完全关闭
							BasicFileAttributes attributes = Files.readAttributes(Paths.get(bf.getPath()),
									BasicFileAttributes.class);

							boolean isRnOK = new File(bf.getPath()).renameTo(new File(bf.getPath()));
							Thread.sleep(200L);
							logger.info(bf.getPath() + "------>" + isRnOK + "-------->" + isRnOK
									+ "\tbyte change?-------->" + (bf.getSize() != attributes.size()));
							// 注意不要把队列撑爆
							if ((bf.getSize() != attributes.size()) || !isRnOK) {
								// 回写增量size
								bf.setSize(attributes.size());
								entry.getValue().add(bf);
								logger.info("----------------------->\tfile no close, wait next copy!");
								Thread.sleep(2000L);
								continue;
							}

							p.put(entry.getKey(), 0L);

							String[] sc = bf.getcPath().split(",");

							for (int j = 0; j < sc.length; j++) {

								String uniSuffix = UUID.randomUUID().toString();

								File tmpFile = new File(sc[j] + "/" + uniSuffix + ".tmp");

								logger.info("----------------------->\tprepare write db for flume_file_log!");

								/*
								 * Map<String, String> mapOj = new
								 * HashMap<String, String>(); mapOj.put("sql",
								 * SqlConstant.INSERT_FLIME_LOGS);
								 * mapOj.put("param",
								 * JSONObject.toJSONString(new Object[] {
								 * bf.getAid(), bf.getSid(), bf.getJid(),
								 * bf.getFid(), bf.getPath().replace("\\", "/"),
								 * bf.getfName(), bf.getSize(), bf.getNum(),
								 * sc[j], bf.getcTime(), bf.getEncode() }));
								 * String resultJson =
								 * HttpPostUtil.post(SqlConstant.EXECUTEURL,
								 * mapOj); JSONObject resObj =
								 * JSONObject.parseObject(resultJson);
								 * 
								 * @SuppressWarnings("static-access")
								 * BaseResponse response =
								 * resObj.parseObject(resultJson,
								 * BaseResponse.class);
								 */

								BufferedReader in = null;
								BaseResponse response = null;
								try {
									URL u = new URL(SqlConstant.EXECUTEURL + "?sql="
											+ URLEncoder.encode(SqlConstant.INSERT_FLIME_LOGS) + "&param="
											+ URLEncoder.encode(JSONObject.toJSONString(new Object[] { bf.getAid(),
													bf.getSid(), bf.getJid(), bf.getFid(),
													bf.getPath().replace("\\", "/"), bf.getfName(), bf.getSize(),
													bf.getNum(), sc[j], bf.getcTime(), bf.getEncode() })));
									InputStream s = u.openStream();

									InputStreamReader reader = new InputStreamReader(s, "UTF-8");
									char[] buff = new char[1024];
									int length = 0;
									String t = "";
									while ((length = reader.read(buff)) != -1) {
										t = t + new String(buff, 0, length);
									}
									JSONObject resObj = JSONObject.parseObject(t);
									response = resObj.parseObject(t, BaseResponse.class);
									System.out.println(
											t + "============================================>>>>>>>>>>>>>>>>>>>>.");
									if (t.indexOf("\"code\":\"00\"") == -1) {
										logger.error("数据库-进程表-插入失败！");
									}

									logger.warn("SQJ-Monitored-Successfully");
								} catch (MalformedURLException e) {
									logger.error(e + "");
								} catch (IOException e) {
									logger.error(e + "");
								} finally {
									try {
										if (in != null)
											in.close();
									} catch (IOException ex) {
										logger.error(ex + "");
									}
								}

								// 增加一个判断，如果resultJson返回错误，联合主键冲突就不能copy文件
								if (response != null && !"00".equals(response.getResponse().getCode())) {

									logger.warn("----------------------->\t" + response.getResponse().getDesc());
									logger.warn("----------------------->\t[" + bf.getfName().replace("\\", "/")
											+ "] write into db ,but data is exist!");

									continue;

								}

								logger.info("----------------------->\tflume_file_log write into db ok!");

								// xls,xlsx
								if (bf.getKind().equals("4.0")) {
									// FileWriteUtil.readXls2Txt(bf.getPath(),
									// tmpFile.getPath(),
									// StringEscapeUtils.unescapeJava(bf.getDecollator()));
									FileWriteUtil.getXlsandx(bf.getPath(), tmpFile.getPath(),
											StringEscapeUtils.unescapeJava(bf.getDecollator()), bf.getColNum(),
											bf.getLine());
									tmpFile.renameTo(new File(sc[j] + "/" + bf.getfName()));
									continue;
								}

								// csv

								System.out.println(bf.getKind() + "=============================" + bf.getPath());
								if (bf.getKind().equals("5.0")) {
									CsvParser.parserCSV2Log(bf.getPath(), tmpFile.getPath(),
											StringEscapeUtils.unescapeJava(bf.getDecollator()), bf.getColNum());
									tmpFile.renameTo(new File(sc[j] + "/" + bf.getfName()));
									continue;
								}

								// 自定义切入插件
								/**
								 * 实现方法被约束为4个参数 args1: 传入源文件绝对路径 args2:
								 * 解析后输出文件绝对路径 args3: 文件分隔符 args4: 文件列长
								 * 
								 * 注意：第三方实现的方法所有异常必去不能自己处理，必须向上抛
								 * 
								 */
								/**
								 * 为实现的修改 1、Flume_File表 增加2个字段 ，外部实现类名，外部实现方法名
								 * 2、kind为10表示为外部实现 3、第三方实现jar必须与fm-final同目录
								 * 4、自定启动命令修改: java -cp
								 * fm-final.jar:migu-ext-etl.jar
								 * cn.migu.file.core.Mapp (后面参数不变...)
								 */
								int ext = 0;

								System.out.println(tmpFile.getPath() + "====================>" + bf.getfName());
								
								if (bf.getKind().equals("10.0")) {

									logger.info(
											"----------------------->\tClass.forName(\"cn.migu.ext.etl.DataHandle4JF\")");

									Class<?> clazz = Class.forName("cn.migu.ext.etl.DataHandle4JF");

									Method method = clazz.getDeclaredMethod(bf.getExtFunName(), String.class,
											String.class);

									method.invoke(clazz.newInstance(), bf.getPath(), tmpFile.getPath());

									logger.info("----------------------->\tmethod.invoke" + bf.getExtFunName() + " ok");

									ext = 1;
								}

								String code = "UTF-8";

								if (bf.getEncode().equals("2.0") || bf.getEncode().equals("4.0")) {

									code = "GBK";
								}

								System.out.println(code + "=======================>");

								// 重新编辑文本
								List<String> r1 = null;

								if (ext == 1) {

									r1 = FileUtils.readLines(tmpFile, code);
								}else{
									
									r1 = FileUtils.readLines(new File(bf.getPath()), code);
								}

								List<String> r2 = new ArrayList<String>();

								int i = 0;

								for (String s : r1) {

									i++;

									r2.add("  " + i + "," + SqlConstant.SERVER_IP + "," + bf.getPath()
											+ "FFFFFa4b6f1e41849c064eef9d0d2a193cd00FFFFF" + s);
								}

								FileUtils.writeLines(tmpFile, "UTF-8", r2);

								// 重名写入Flume监控目录
								tmpFile.renameTo(new File(sc[j] + "/" + bf.getfName()));

								logger.info("----------------------->\t[" + bf.getPath() + "] write [" + sc[j] + "/"
										+ bf.getfName() + "] ok!" + " size: " + bf.getSize());

								// System.out.println(bf.getPath().replace("\\",
								// "/")+"---"+sc[j]+"---"+entry.getKey());

								logger.info("----------------------->write into JMX");

								// 写入JMX监控
								/**
								 * 1.就自定义jar而言只要知道每个正则对应的采集频率即可 2.同样就自定义jar而言只要在
								 * flume_file表中，增加一个频率字段即可 3.对于其它类型的jar，需要另行定义频率
								 * 4.对于监控进程，通过不同的ip和端口号连接到所有运行的jar
								 * 5.监控jar本来就应该更跟任何业务无关，只要对约定好的域分析即可
								 * 6.约定好的域是一个map对象，map对象也是约定好的，只要看其变化的频率
								 */

								
								MetricInfo metricInfo = new MetricInfo();
								metricInfo.setFileNum(1L);
								metricInfo.setRecordNum((long)bf.getNum());

								metricInfo.setLastUpdateTime(System.currentTimeMillis());

								Monitor.put(bf.getPath().replace("\\", "/") + sc[j] + entry.getKey(), metricInfo);

								logger.info("----------------------->write into JMX ok! ");
							}
						}
					} catch (Exception e) {
						// logger.error(e.getMessage());

						e.printStackTrace();
					}
				}
			}).start();
		}
	}

	// // 心跳线程
	// public static synchronized void startHeartbeat(final List<String> hf) {
	//
	// Timer timer = new Timer();
	//
	// timer.scheduleAtFixedRate(new TimerTask() {
	// public void run() {
	//
	// for (String h : hf) {
	//
	// try {
	//
	// FileUtils.deleteQuietly(new File(h + "/migu.heartbeat"));
	//
	// Thread.sleep(1000L);
	//
	// FileUtils.write(new File(h + "/migu.heartbeat"),
	// System.currentTimeMillis() + "", "UTF-8",
	// false);
	//
	// logger.info("----------------------->\t插入心跳文件[" + h + "/migu.heartbeat" +
	// "]成功!");
	//
	// } catch (IOException e) {
	// logger.error("------------------>\t 插入心跳文件失败!");
	// } catch (InterruptedException e) {
	// logger.error("------------------>\t 插入心跳文件失败!");
	// }
	//
	// }
	//
	// }
	// }, 1000, 1000 * 30L);// 延迟，间隔毫秒
	//
	// }
}
