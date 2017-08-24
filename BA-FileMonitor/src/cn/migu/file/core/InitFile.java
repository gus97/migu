/**
 * 
 */
package cn.migu.file.core;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.DirectoryFileFilter;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.alibaba.fastjson.JSONObject;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

/**
 * @author gus97 2016年11月29日 上午9:40:53
 */
public class InitFile {

	static private Logger logger = LoggerFactory.getLogger(InitFile.class);

	static private Map<Pattern, Pace> map = new HashMap<Pattern, Pace>();

	@SuppressWarnings("static-access")
	public static void initFileMonitor(Map<String, List<Pace>> rp, String aid, String sid, String jid) throws Exception {

		for (Entry<String, List<Pace>> entry1 : rp.entrySet()) {

			for (Pace pace : entry1.getValue()) {

				map.put(pace.getPattern(), pace);
			}
		}

		// 远程调用服务QUERY_FLUME_LOGS
		Gson gson = new Gson();
		Map<String, String> mapOj = new HashMap<String, String>();
		mapOj.put("sql", SqlConstant.QUERY_FLUME_LOGS);
		String resultJson = HttpPostUtil.post(SqlConstant.QUERYFORLISTURL, mapOj);
		mapOj.put("param", JSONObject.toJSONString(new Object[] { aid, sid, jid,SqlConstant.sqlRollTime }));
		resultJson = HttpPostUtil.post(SqlConstant.QUERYFORLISTURL, mapOj);
		JSONObject resObj = JSONObject.parseObject(resultJson);
		BaseResponse response = resObj.parseObject(resultJson, BaseResponse.class);
		resObj = JSONObject.parseObject(resultJson);
		response = resObj.parseObject(resultJson, BaseResponse.class);
		List<Map<String, Object>> resQueryFL = null;
		// 列表为空时返回[]
		String arrStr = response.getResponse().getContent();
		if (!"[]".equals(arrStr)) {

			resQueryFL = gson.fromJson(arrStr, new TypeToken<List<Map<String, Object>>>() {
			}.getType());
		} else {
			resQueryFL = null;
		}

		Set<String> collectFiles = new HashSet<String>();
		
		

		Double rt = Math.ceil((System.currentTimeMillis() - SqlConstant.rollTime) / 1000 / 60 / 60D);
		// 查询所有已收集过的文件
		if (resQueryFL != null && resQueryFL.size() != 0) {
			for (Map<String, Object> map : resQueryFL) {
				String fname = (String) map.get("FNAME");

				collectFiles.add(StringUtils.replace(fname, "\\", "/"));
			}
			logger.info("----------------------->\tbefore " + rt + " hour, query db file nums is : " + resQueryFL.size());
		} else {
			logger.info("----------------------->\t" + rt + "no files in db");
		}
		
		for (Entry<String, List<Pace>> entry2 : rp.entrySet()) {

			logger.info("----------------------->\tscan sourceDir[" + entry2.getKey() + "],roll back " + rt + " hour");

			long backTime = SqlConstant.rollTime;
			
			System.out.println(entry2.getKey()+"--------------------------->");
			
			Collection<File> listFiles = FileUtils.listFiles(new File(entry2.getKey()),
					FileFilterUtils.ageFileFilter(backTime, false), DirectoryFileFilter.INSTANCE);

			Set<String> fileSet = new HashSet<String>();

			for (File file : listFiles) {

				fileSet.add(file.getPath().replace("\\", "/"));
			}

			logger.info("----------------------->\tsourceDir [" + entry2.getKey() + "] before " + rt + " hour, add new File nums is : "
					+ fileSet.size());
			
			logger.info(fileSet+"$$$$$$$$$$$$$$"+collectFiles);

			fileSet.removeAll(collectFiles);

			logger.info("----------------------->\tsourceDir [" + entry2.getKey() + "] " + rt + "hour difference: \n" + fileSet);

			logger.info("\n----------------------->\tsourceDir [" + entry2.getKey() + "] into file quene !");

			for (String fn : fileSet) {

				try {
					int bl = 0;

					// 统一路径下的多个表达式，不同的表达式写不同的采集路径
					for (Entry<Pattern, Pace> entry : map.entrySet()) {

						Matcher matcher = entry.getKey().matcher(fn.split("/")[fn.split("/").length - 1]);

						if (matcher.matches()) {

							bl++;

							BasicFileAttributes attributes = Files.readAttributes(Paths.get(fn),
									BasicFileAttributes.class);

							String uniSuffix = "." + UUID.randomUUID().toString();

							Pace p = entry.getValue();

							BlockingQueue4Files.bqf.get(entry.getKey())
									.add(new BlockingQueneFileInfo(p.getAid(), p.getSid(), p.getJid(), p.getOid(), fn,
											fn.split("/")[fn.split("/").length - 1] + uniSuffix, attributes.size(),
											CodeDetectUtil.fileLength(fn), p.getCollectPath(),
											FileWriteUtil.getLongDateString(attributes.creationTime().toMillis()),
											p.getCharacter(),p.getKind(),p.getDecollator(),p.getColNum(),p.getLine(),p.getExtFunName()));
							logger.info("----------------------->\tsource file [" + fn + "] write [" + entry.getKey() + "] quene ok！");

							break;
						}
					}
					if (bl == 0) {
						logger.warn("----------------------->\t[" + fn + "] no regExp!");
					}

				} catch (Exception e) {

					logger.error("ERROR: " + fn + "\t----------------------->\t", e);

				}
			}
		}
	}
}
