package cn.migu.file.core;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.monitor.FileAlterationListenerAdaptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MiguFileListener extends FileAlterationListenerAdaptor {

	static private Logger logger = LoggerFactory.getLogger(MiguFileListener.class);

	private List<Pace> lp;

	private Map<Pattern, Pace> map;

	public List<Pace> getLp() {
		return lp;
	}

	public void setLp(List<Pace> lp) {
		this.lp = lp;
	}

	public MiguFileListener(List<Pace> lp) {

		this.lp = lp;

		map = new HashMap<Pattern, Pace>();

		for (Pace pace : lp) {

			map.put(pace.getPattern(), pace);
		}
	}

	@Override
	public void onFileCreate(File file) {

		logger.info("\n----------------------->\tonFileCreate : " + file.getPath());
		
		
//		try {
//			Thread.sleep(100000L);
//		} catch (InterruptedException e1) {
//			// TODO Auto-generated catch block
//			e1.printStackTrace();
//		}

//		if ("migu.heartbeat".equals(file.getName())) {
//			
//			logger.info("----------------------->\t发现心跳文件: " + file.getName());
//			for (Pace pace : this.lp) {
//				try {
//					FileUtils.copyFileToDirectory(new File(file.getPath()), new File(pace.getCollectPath()), true);
//					logger.info("----------------------->\t" + "["+file.getPath() +"]-->["+ pace.getCollectPath() + "/" + file.getName()
//							+ "]\t心跳文件copy成功! ");
//				} catch (IOException e) {
//					logger.error("ERROR: 心跳文件copy失败!" + file.getPath() + "\t----------------------->\t", e);
//				}
//
//			}
//			return;
//		}

		try {
			int bl = 0;

			//统一路径下的多个表达式，不同的表达式写不同的采集路径
			for (Entry<Pattern, Pace> entry : map.entrySet()) {

				Matcher matcher = entry.getKey().matcher(file.getName());

				if (matcher.matches()) {
					
					bl++;

					BasicFileAttributes attributes = Files.readAttributes(Paths.get(file.getPath()),
							BasicFileAttributes.class);

					String uniSuffix = "." + UUID.randomUUID().toString();

					Pace p = entry.getValue();

					BlockingQueue4Files.bqf.get(entry.getKey())
							.add(new BlockingQueneFileInfo(p.getAid(), p.getSid(), p.getJid(), p.getOid(),
									file.getPath(), file.getName() + uniSuffix, attributes.size(),
									CodeDetectUtil.fileLength(file.getPath()), p.getCollectPath(),
									FileWriteUtil.getLongDateString(attributes.creationTime().toMillis()), p.getCharacter(),p.getKind(),p.getDecollator(),p.getColNum(),p.getLine(),p.getExtFunName()));
					logger.info("----------------------->\tsource file [" + file.getPath() + "] write into [" + entry.getKey() + "] quene！");

					break;
				}
			}
			if (bl == 0) {
				logger.warn("----------------------->\t[" + file.getPath() + "] no EXP find!");
			}

		} catch (Exception e) {

			logger.error("ERROR: " + file.getPath() + "\t----------------------->\t", e);

		}
	}
}
