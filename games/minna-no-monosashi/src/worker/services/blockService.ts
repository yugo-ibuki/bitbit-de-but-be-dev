import type {
  PublicBlockDetail,
  PublicBlockSummary,
} from "../../shared/contracts";
import { BlockRepository } from "../repositories/blockRepository";

export class BlockService {
  private readonly repository: BlockRepository;

  constructor(db: D1Database) {
    this.repository = new BlockRepository(db);
  }

  listPublic(): Promise<PublicBlockSummary[]> {
    return this.repository.listPublic();
  }

  findPublicBySlug(slug: string): Promise<PublicBlockDetail | null> {
    return this.repository.findPublicBySlug(slug);
  }
}
